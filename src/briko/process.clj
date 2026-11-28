;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process
  "Runs a program as an argv, never through a shell, and comes back with an
   answer within a deadline unless the caller explicitly declares the run
   unbounded.

   `run` returns {:exit :out :err :timed-out? :launched? :out-truncated?
   :err-truncated?}, plus :out-bytes and :out-error when stdout goes to a
   file. It does not throw when the program cannot be started or overruns
   its deadline:

     - a program that cannot be launched (missing, not executable, a
       working directory that does not exist, a stdin or stdout file that
       cannot be opened) answers exit 127, the shell's convention for
       'command not found', with the reason in :err and :launched? false;
     - a program still running at the deadline, or whose output is still
       open then, answers exit 124, the convention of coreutils `timeout`,
       with :timed-out? true. Its whole process tree is killed with
       SIGKILL first.

   The deadline covers the program and its output: a run is finished once
   the program has exited and every process holding its stdout or stderr
   has closed it, so a background child that keeps stdout open answers
   124 at the deadline, every time. The output travels through named pipes
   in a private temporary directory, read by this process alone: Java's
   own process pipes are closed by the JDK as soon as the program exits,
   whatever a descendant still holds, which would turn the outcome into a
   race between that exit and the reading thread. Creating the pipes needs
   `mkfifo` on the PATH; a run that cannot create them answers exit 127
   with :launched? false, as for a program that cannot be launched. On a
   JVM with native access enabled for our code (for example
   --enable-native-access=ALL-UNNAMED), they are created in this process
   through mkfifo(3) instead, which saves starting a program per run.

   Stdout written to a file ({:out {:file path}}) travels through the same
   pipe and is copied into the file by this process, so a detached child
   holding stdout makes the run 124 exactly as when stdout is captured,
   and nothing reaches the file after `run` returned.

   With {:timeout-ms :none} the run has no deadline: it lasts until the
   program and every holder of its output are done, or until the waiting
   thread is interrupted, which kills the tree as a timeout would.

   An :on-start function is called once the program has started, on the
   thread that called `run`, with a handle {:pid :stop}: the program's
   pid and a function that kills the program, its tree and every holder
   of its output exactly as a timeout does, from any thread, such as a
   JVM shutdown hook. A stopped run returns with the program's exit
   (137 after SIGKILL) and :stopped? true. :stop is idempotent and does
   nothing once the run has timed out, been interrupted or returned. A
   callback that throws has the program stopped the same way, and `run`
   refuses with ::on-start-failed, the callback's exception as cause.

   On a timeout the program is killed with SIGKILL, then every descendant
   seen while it ran, then, where /proc exists, every process still
   holding one of its output pipes, also one started and detached between
   two samples. Descendants are sampled while the program runs, first
   after 10 ms and then at doubling intervals up to every 500 ms. A
   descendant that was detached between two samples, or before the first,
   and does not hold the output is out of reach, because Java cannot put
   the program in a process group of its own; it neither delays the run
   nor is killed by it.

   Captured output is kept up to a byte limit per stream, its first bytes
   by default or its last ones with :out-keep or :err-keep :tail. Past the
   limit the stream is still read, so the program never blocks on a full
   pipe, but the excess is discarded and :out-truncated? or
   :err-truncated? is set. Output is decoded as UTF-8, replacing malformed
   bytes; a tail that was cut starts at the first whole character.

   `start` runs a program alongside the caller, such as an SSH tunnel or a
   port-forward, through the same named pipes, and returns a handle:
   `alive?`, `output` (the bounded end of each stream, or stdout in a
   file), `wait-ready` (until a line of output matches a regex or a
   loopback port accepts, within a deadline) and `close` (kills the tree
   and every holder of the output, idempotent, returns the exit). Every
   started program still running is stopped by one JVM shutdown hook
   unless the caller opts out with :stop-on-exit? false.

   Invalid arguments are a programming error and throw an ex-info whose
   :type is in `refusal-types`; an unknown option is refused, so a
   misspelt :timeout-ms cannot fall back to the default unnoticed, and so
   is a limit or keep option for a stream that is not captured. A thread
   interrupted while it waits kills the tree and rethrows the
   InterruptedException with its interrupt flag set."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str])
  (:import (java.io ByteArrayOutputStream File FileInputStream IOException InputStream OutputStream
                    RandomAccessFile)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path StandardOpenOption)
           (java.util Arrays List)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a run uses where the caller gives none.

   - :timeout-ms, the deadline from launch in milliseconds, or :none for a
     run without one: 60000. Long enough for a password manager waiting on
     a hardware key, short enough that a wedged tool cannot hold a
     scheduled run for minutes.
   - :extra-env, variables added to the inherited environment: none.
   - :unset-env, variables removed from it: none.
   - :dir, the working directory: the current one.
   - :in, what the program reads on stdin: nil closes stdin at once, so a
     program waiting for input reads end of file instead of hanging. A
     string or byte array is written and stdin closed after it; {:file
     path} reads the regular file at path; :tty reads /dev/tty, the
     controlling terminal, and answers 127 where there is none.
   - :out, where stdout goes: nil captures it into :out; {:file path}
     writes it into the file at path, created owner-only or truncated, and
     refused when path is a symbolic link or not a regular file.
   - :max-out-bytes, the stdout captured: 1 MiB.
   - :max-err-bytes, the stderr captured: 64 KiB.
   - :out-keep and :err-keep, which part of a stream past its limit is
     kept: :head, its first bytes, or :tail, its last ones: :head.
   - :inherit-err?, whether stderr goes to this process's stderr instead
     of being captured: false.
   - :on-start, a function called with the handle {:pid :stop} once the
     program has started, or nil: nil. With one, the result also carries
     :stopped?, whether :stop took effect before the run finished."
  {:timeout-ms 60000
   :extra-env {}
   :unset-env []
   :dir nil
   :in nil
   :out nil
   :max-out-bytes 1048576
   :max-err-bytes 65536
   :out-keep :head
   :err-keep :head
   :inherit-err? false
   :on-start nil})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-argv ::invalid-options ::on-start-failed ::invalid-handle ::exited-before-ready ::ready-timeout})

(defn- refuse!
  [type message data]
  (throw (ex-info message (assoc data :type type))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

;; Arguments

(defn- no-nul?
  [^String s]
  (neg? (.indexOf s (int 0))))

(defn- env-name?
  [x]
  (and (string? x) (not (str/blank? x)) (no-nul? x) (not (str/includes? x "="))))

(defn- int-at-most?
  [limit x]
  (and (int? x) (<= 0 (long x) (long limit))))

(defn- path-like?
  [p]
  (or (and (string? p) (not (str/blank? p)) (no-nul? p))
      (instance? File p)
      (instance? Path p)))

(defn- file-spec?
  [x]
  (and (map? x) (= #{:file} (set (keys x))) (path-like? (:file x))))

(def ^:private option-checks
  {:timeout-ms [(fn [x] (or (= :none x) (and (int-at-most? Integer/MAX_VALUE x) (pos? (long x)))))
                "a positive int of at most Integer/MAX_VALUE milliseconds, or :none"]
   :extra-env [(fn [m] (and (map? m) (every? (fn [[k v]] (and (env-name? k) (string? v) (no-nul? v))) m)))
               "a map from variable name to string value"]
   :unset-env [(fn [xs] (and (coll? xs) (not (map? xs)) (every? env-name? xs)))
               "a collection of variable names"]
   :dir [(fn [d] (or (nil? d) (string? d) (instance? File d) (instance? Path d)))
         "nil, a string, a File or a Path"]
   :in [(fn [x] (or (nil? x) (string? x) (bytes? x) (= :tty x) (file-spec? x)))
        "nil, a string, a byte array, :tty or {:file path}"]
   :out [(fn [x] (or (nil? x) (file-spec? x))) "nil or {:file path}"]
   :max-out-bytes [(partial int-at-most? (dec Integer/MAX_VALUE)) "an int from 0 below Integer/MAX_VALUE"]
   :max-err-bytes [(partial int-at-most? (dec Integer/MAX_VALUE)) "an int from 0 below Integer/MAX_VALUE"]
   :out-keep [#{:head :tail} ":head or :tail"]
   :err-keep [#{:head :tail} ":head or :tail"]
   :inherit-err? [boolean? "a boolean"]
   :on-start [(fn [f] (or (nil? f) (fn? f))) "nil or a function of one argument"]
   :stop-on-exit? [boolean? "a boolean"]})

(def ^:private contradictions
  "[option given, options it rules out, why], each refused when given
   together, because the second would be silently ignored."
  [[:out #{:max-out-bytes :out-keep} "stdout written to a file is not captured"]
   [:inherit-err? #{:max-err-bytes :err-keep} "inherited stderr is not captured"]])

(defn- checked-options
  "opts merged over defaults, refused when an option is unknown to
   defaults, invalid, or ruled out by another one given."
  [opts defaults contradictions]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [given opts
        opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks
            :when (contains? defaults k)]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k})))
    (when-let [both (seq (filter (set (:unset-env opts)) (keys (:extra-env opts))))]
      (refuse! ::invalid-options (str "variables both set and unset: " (pr-str (vec both))) {:option :unset-env}))
    (doseq [[k ruled-out why] contradictions
            :when (get opts k)
            :let [clash (sort (filter (partial contains? given) ruled-out))]
            :when (seq clash)]
      (refuse! ::invalid-options (str k " rules out " (pr-str (vec clash)) ": " why) {:option (first clash)}))
    opts))

(defn- options
  [opts]
  (checked-options opts defaults contradictions))

(defn- check-argv!
  [argv]
  (when-not (and (sequential? argv)
                 (seq argv)
                 (every? string? argv)
                 (every? no-nul? argv)
                 (not (str/blank? (first argv))))
    ;; The argv is not echoed: it may carry a value the caller considers
    ;; private.
    (refuse! ::invalid-argv
             "the argv must be a non-empty sequence of strings without NUL, starting with a program name"
             {})))

(defn- environment
  [{:keys [extra-env unset-env]}]
  (apply dissoc (merge (into {} (System/getenv)) extra-env) unset-env))

;; Deadlines

(defn- deadline-of
  "The deadline as a System/nanoTime value, or nil for a run without one."
  [timeout-ms]
  (when-not (= :none timeout-ms)
    (+ (System/nanoTime) (* 1000000 (long timeout-ms)))))

(defn- remaining-ms
  "Milliseconds until deadline, never negative; Long/MAX_VALUE without one."
  ^long [deadline]
  (if deadline
    (max 0 (quot (- (long deadline) (System/nanoTime)) 1000000))
    Long/MAX_VALUE))

(def ^:private setup-ms
  "The bound on creating the pipes of a run without a deadline."
  60000)

;; Sinks: where a stream's bytes go. Each is {:consume (fn [buf n]) and
;; :finish (fn [])}. consume answers whether to go on reading; finish
;; stops it accepting bytes and returns its part of the result. Both run
;; under the sink's lock, so a reader thread abandoned to a pipe that a
;; process out of reach still holds can neither change a result already
;; returned nor write into a file after `run` came back.

(defn- locked-sink
  "A sink of consume and finish under one lock. With so-far, the sink also
   answers :peek, so-far called under the lock: its part of the result
   so far, without finishing."
  ([consume finish] (locked-sink consume finish nil))
  ([consume finish so-far]
   (let [lock (Object.)
         open (volatile! true)]
     (cond-> {:consume (fn [buf n] (locking lock (and @open (consume buf n))))
              :finish (fn [] (locking lock (vreset! open false) (finish)))}
       so-far (assoc :peek (fn [] (locking lock (so-far))))))))

(defn- head-sink
  "Keeps the first limit bytes."
  [limit]
  (let [sink (ByteArrayOutputStream.)
        limit (long limit)
        dropped (volatile! false)]
    (locked-sink
     (fn [^bytes buf n]
       (let [n (long n)
             kept (min n (max 0 (- limit (.size sink))))]
         (when (pos? kept) (.write sink buf 0 (int kept)))
         (when (< kept n) (vreset! dropped true))
         true))
     (fn [] {:bytes (.toByteArray sink) :dropped? @dropped}))))

(defn- continuation-byte?
  [b]
  (= 0x80 (bit-and (long b) 0xC0)))

(defn- from-whole-character
  "bs without the UTF-8 continuation bytes it starts with, at most three,
   so a tail cut in the middle of a character starts at the next one."
  ^bytes [^bytes bs]
  (let [skip (count (take-while continuation-byte? (take 3 bs)))]
    (Arrays/copyOfRange bs (int skip) (alength bs))))

(defn- last-bytes
  ^bytes [^bytes bs limit]
  (let [len (alength bs)
        limit (long limit)]
    (if (> len limit)
      (Arrays/copyOfRange bs (int (- len limit)) len)
      bs)))

(defn- tail-sink
  "Keeps the last limit bytes. The buffer grows to at most limit plus
   max(limit, 8192) bytes before it is cut back to the last limit, so
   memory stays bounded and each byte is copied a bounded number of times."
  [limit]
  (let [sink (ByteArrayOutputStream.)
        limit (long limit)
        slack (max limit 8192)
        total (volatile! 0)
        snapshot (fn []
                   (let [dropped? (> (long @total) limit)
                         kept (last-bytes (.toByteArray sink) limit)]
                     {:bytes (if dropped? (from-whole-character kept) kept)
                      :dropped? dropped?}))]
    (locked-sink
     (fn [^bytes buf n]
       (let [n (long n)]
         (vswap! total + n)
         (cond
           (zero? limit) nil

           (>= n limit)
           (do (.reset sink)
               (.write sink buf (int (- n limit)) (int limit)))

           :else
           (do (.write sink buf 0 (int n))
               (when (> (.size sink) (+ limit slack))
                 (let [kept (last-bytes (.toByteArray sink) limit)]
                   (.reset sink)
                   (.write sink kept 0 (alength kept))))))
         true))
     snapshot
     snapshot)))

(defn- capture-sink
  [keep limit]
  (if (= :tail keep) (tail-sink limit) (head-sink limit)))

(defn- file-sink
  "Writes every byte to out. A failed write closes out, records the reason
   and stops reading, so the program's next write fails instead of being
   discarded."
  [^OutputStream out]
  (let [written (volatile! 0)
        error (volatile! nil)
        close! (fn []
                 (try (.close out)
                      (catch IOException e
                        (when-not @error (vreset! error (str "could not close the output file: " (ex-message e)))))))]
    (locked-sink
     (fn [^bytes buf n]
       (try
         (.write out buf 0 (int n))
         (vswap! written + (long n))
         true
         (catch IOException e
           (vreset! error (str "could not write the output file: " (ex-message e)))
           (close!)
           false)))
     (fn []
       (close!)
       {:written @written :error @error})
     (fn [] {:written @written :error @error}))))

;; Streams

(defn- daemon!
  "Starts f on a daemon thread, so a thread blocked on a pipe that some
   detached grandchild holds open never keeps the JVM from exiting."
  ^Thread [f]
  (doto (Thread. ^Runnable f)
    (.setDaemon true)
    (.start)))

(defn- drain!
  "Reads in to its end on a daemon thread, handing every chunk to the
   sink's consume, until it answers false, then calls the sink's :eof, if
   it has one."
  ^Thread [^InputStream in {:keys [consume eof]}]
  (daemon!
   (fn []
     (let [buf (byte-array 8192)]
       (try
         (loop []
           (let [n (.read in buf)]
             (when (and (pos? n) (consume buf n))
               (recur))))
         (catch IOException _ nil)
         (finally
           (try (.close in) (catch IOException _ nil))
           (when eof (eof))))))))

(defn- feed!
  "Writes input to the program's stdin on a daemon thread and closes it. A
   program that exits without reading closes the pipe; that is its
   choice, not an error."
  ^Thread [^OutputStream stdin input]
  (daemon!
   (fn []
     (try
       (when (or (string? input) (bytes? input))
         (.write stdin (if (string? input) (.getBytes ^String input StandardCharsets/UTF_8) ^bytes input))
         (.flush stdin))
       (catch IOException _ nil)
       (finally
         (try (.close stdin) (catch IOException _ nil)))))))

;; Files

(def ^:private tty-path
  "The controlling terminal, read by {:in :tty}."
  "/dev/tty")

(defn- input-file
  "The File the program reads stdin from, or nil when stdin is a pipe.
   Throws an IOException naming the reason when it is not a regular file,
   because opening a named pipe or a device other than the terminal could
   block before the deadline is armed."
  ^File [in]
  (cond
    (= :tty in) (File. ^String tty-path)
    (map? in) (let [f (fs/file (:file in))]
                (when-not (Files/isRegularFile (.toPath f) (make-array LinkOption 0))
                  (throw (IOException. (str "the stdin file " f " is not a regular file"))))
                f)
    :else nil))

(def ^:private output-file-options
  (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/TRUNCATE_EXISTING
                          StandardOpenOption/WRITE LinkOption/NOFOLLOW_LINKS]))

(defn- open-output-file
  "Opens the file at path for writing, created owner-only when it does not
   exist, truncated when it is a regular file. Throws an IOException when
   it is a symbolic link or exists and is not a regular file, since
   opening a named pipe for writing blocks until a reader arrives."
  ^OutputStream [path]
  (let [p (fs/path path)]
    (when (and (Files/exists p (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
               (not (Files/isRegularFile p (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))))
      (throw (IOException. (str "the stdout file " p " is not a regular file"))))
    (when-not (Files/exists p (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
      (fs/create-file p {:posix-file-permissions "rw-------"}))
    (Files/newOutputStream p ^"[Ljava.nio.file.OpenOption;" output-file-options)))

;; Output pipes

(def ^:private mkfifo-program
  "The program that creates the named pipes where they are not created
   in this process."
  "mkfifo")

(def ^:private native-mkfifo
  "A function creating one owner-only named pipe in this process, from
   briko.mkfifo, or nil where that cannot be used without a warning or
   at all: under babashka, on a JVM without native access enabled for
   our code, and where the foreign function API is missing."
  (delay
    (when-not (System/getProperty "babashka.version")
      (try
        ((requiring-resolve 'briko.mkfifo/maker))
        ;; A runtime without the foreign function API fails with an Error
        ;; (a native image), and every failure means the program is used.
        #_{:splint/disable [lint/catch-throwable]}
        (catch Throwable _ nil)))))

(defn- run-mkfifo!
  "Creates a named pipe, readable and writable by the owner only, at each
   of paths with the mkfifo program, within the deadline. Returns nil, or
   the reason it could not."
  [paths deadline]
  (try
    (let [proc (.start (doto (ProcessBuilder. ^List (into [mkfifo-program "-m" "600"] paths))
                         (.redirectErrorStream true)))]
      (.close (.getOutputStream proc))
      (if (.waitFor proc (max 1 (remaining-ms deadline)) TimeUnit/MILLISECONDS)
        (when-not (zero? (.exitValue proc))
          (str "mkfifo failed: " (str/trim (String. (.readAllBytes (.getInputStream proc)) StandardCharsets/UTF_8))))
        (do (.destroyForcibly proc)
            "mkfifo overran the deadline")))
    (catch IOException e
      (str "mkfifo could not run: " (ex-message e)))))

(defn- make-fifos!
  "Creates a named pipe, readable and writable by the owner only, at each
   of paths, in this process where native-mkfifo is available and with
   the mkfifo program within the deadline otherwise. Returns nil, or the
   reason it could not."
  [paths deadline]
  (if-let [make! @native-mkfifo]
    (some make! paths)
    (run-mkfifo! paths deadline)))

(defn- open-reader
  "Opens the read end of the named pipe at path. A writer must hold the
   pipe already, or the open blocks until one arrives."
  ^InputStream [^String path]
  (FileInputStream. path))

(defn- open-keeper
  "Opens the named pipe at path for reading and writing, which does not
   block. While it is open, the program's redirect can open the pipe for
   writing and the reader can open it for reading, neither waiting for
   the other, and the pipe cannot reach end of file before the reader
   holds it."
  ^RandomAccessFile [^String path]
  (RandomAccessFile. path "rw"))

(defn- close-quietly
  [^java.io.Closeable c]
  (when c
    (try (.close c) (catch IOException _ nil))))

(defn- output-holders
  "The pids of other processes holding a file below dir open, read from
   /proc; empty where /proc does not exist or a process's descriptors are
   not ours to read."
  [^String dir]
  (let [self (.pid (java.lang.ProcessHandle/current))
        prefix (str dir File/separator)]
    (distinct
     (for [^File proc-dir (or (.listFiles (File. "/proc")) [])
           :let [pid (parse-long (.getName proc-dir))]
           :when (and pid (not= pid self))
           ^File fd (or (.listFiles (File. proc-dir "fd")) [])
           :let [target (try (str (Files/readSymbolicLink (.toPath fd))) (catch Exception _ ""))]
           :when (str/starts-with? target prefix)]
       pid))))

(defn- kill-holders!
  "Kills every process holding a file below dir open with SIGKILL, looking
   again while a round still found one, so a holder that forked before it
   died is found too."
  [dir]
  (loop [rounds 5]
    (let [pids (output-holders dir)]
      (when (and (seq pids) (pos? rounds))
        (doseq [pid pids]
          (let [^java.util.Optional h (java.lang.ProcessHandle/of (long pid))]
            (when (.isPresent h)
              (.destroyForcibly ^java.lang.ProcessHandle (.get h)))))
        (Thread/sleep 20)
        (recur (dec rounds))))))

;; The process tree

(def ^:private children-files?
  "Whether Linux lists each task's children in /proc/<pid>/task/<tid>/children,
   which it does when built with CONFIG_PROC_CHILDREN."
  (delay
    (let [pid (.pid (java.lang.ProcessHandle/current))]
      (.isFile (File. (str "/proc/" pid "/task/" pid "/children"))))))

(defn- child-pids
  "The pids /proc lists as children of any thread of the process pid;
   empty once it has exited. Throws an IOException when a thread's list
   cannot be read."
  [pid]
  (into []
        (comp (mapcat (fn [^File task]
                        (-> (Files/readAllBytes (.toPath (File. task "children")))
                            (String. StandardCharsets/US_ASCII)
                            str/trim
                            (str/split #" "))))
              (keep parse-long))
        (.listFiles (File. (str "/proc/" pid "/task")))))

(defn- children-in-proc
  "The live children of handle, from /proc. A pid is kept only when the
   process now holding it names handle as its parent, so a pid reused
   after its process exited is never taken for a child."
  [^java.lang.ProcessHandle handle]
  (keep (fn [pid]
          (let [^java.util.Optional child (java.lang.ProcessHandle/of (long pid))]
            (when (.isPresent child)
              (let [^java.lang.ProcessHandle child (.get child)
                    ^java.util.Optional parent (.parent child)]
                (when (and (.isPresent parent) (= handle (.get parent)))
                  child)))))
        (child-pids (.pid handle))))

(defn- descendants-of
  "The program's live descendants. Where /proc lists children, walking the
   program's own tree costs a few file reads per descendant;
   ProcessHandle.descendants reads every process on the host, which takes
   milliseconds per sample with hundreds of processes, and is used where
   /proc lists no children or a list could not be read."
  [^Process proc]
  (or (when @children-files?
        (try
          (loop [found [] pending [(.toHandle proc)]]
            (if-let [h (first pending)]
              (let [children (children-in-proc h)]
                (recur (into found children) (into (subvec pending 1) children)))
              found))
          ;; A thread that exited between listing and reading, or a list
          ;; that cannot be read: this sample walks the process table.
          (catch IOException _ nil)))
      (iterator-seq (.iterator (.descendants (.toHandle proc))))))

(defn- sample!
  "Adds the program's live descendants to seen, an atom holding a set of
   ProcessHandles, and drops those that have exited, so a long run that
   starts many short children does not accumulate them."
  [^Process proc seen]
  (let [live (descendants-of proc)]
    (swap! seen (fn [s]
                  (into (into #{} (filter (fn [^java.lang.ProcessHandle h] (.isAlive h))) s)
                        live)))))

(defn- kill-tree!
  "Kills the program and every descendant seen or alive now with SIGKILL,
   the program first, so it starts no further child."
  [^Process proc seen]
  (sample! proc seen)
  (.destroyForcibly proc)
  (run! (fn [^java.lang.ProcessHandle h] (.destroyForcibly h)) @seen))

(defn- kill-all!
  "Kills the program's tree and every process holding one of its output
   pipes below pipe-dir: what a timeout, an interruption and a stop do."
  [proc seen pipe-dir]
  (kill-tree! proc seen)
  (kill-holders! pipe-dir))

(defn- controller
  "The state of one launched run and the only way to end it early. Every
   transition runs under one lock: :running becomes :done when the run
   finished, or :stopped, :timed-out, :interrupted or :on-start-failed,
   each of which kills the tree and the holders while the lock is held, so
   the run cannot return and remove its pipes in the middle of a kill.
   `halt!` answers whether this call ended the run."
  [proc seen pipe-dir]
  (let [lock (Object.)
        state (volatile! :running)]
    {:state (fn [] @state)
     :halt! (fn [to]
              (locking lock
                (when (= :running @state)
                  (vreset! state to)
                  (kill-all! proc seen pipe-dir)
                  true)))
     :done! (fn []
              (locking lock
                (when (= :running @state)
                  (vreset! state :done))
                @state))}))

(def ^:private grace-ms
  "How long a timed-out run waits for the output its killed tree wrote."
  200)

(defn- await!
  "Waits until the program has exited and both output threads finished, or
   the deadline passed, or stopped? answers true, sampling descendants with
   a growing interval while the program runs. Returns :finished,
   :deadline or :stopped.

   The first sample is taken only after the program ran for one interval:
   the samples serve only to kill a tree that outlives its deadline or is
   stopped, which a program that has exited cannot, and where /proc lists
   no children a sample walks the whole process table, about 15 ms with
   600 processes."
  [^Process proc ^Thread out-thread ^Thread err-thread deadline seen stopped?]
  (loop [interval 10]
    (let [left (remaining-ms deadline)
          step (min left (long interval))]
      (cond
        (stopped?)
        :stopped

        (and (not (.isAlive proc)) (not (.isAlive out-thread)) (not (.isAlive err-thread)))
        :finished

        (zero? left)
        :deadline

        :else
        (do (if (.isAlive proc)
              (when-not (.waitFor proc step TimeUnit/MILLISECONDS)
                (sample! proc seen))
              (.join (if (.isAlive out-thread) out-thread err-thread) step))
            (recur (min 500 (* 2 interval))))))))

(defn- text
  ^String [^bytes bs]
  (String. bs StandardCharsets/UTF_8))

(defn- launch-failure
  [^String reason opts]
  (cond-> {:exit 127
           :out ""
           :err reason
           :timed-out? false
           :launched? false
           :out-truncated? false
           :err-truncated? false}
    (:out opts) (assoc :out nil :out-bytes 0 :out-error nil)))

(defn- reason-of
  ^String [^Exception e]
  (or (ex-message e) (.getName (class e))))

(defn- launch
  "Starts the program with stdout, and stderr unless it is inherited,
   redirected to the named pipes at paths, and opens their read ends.
   Returns {:proc :readers}, or {:failure reason} when it did not start."
  [argv {:keys [dir in inherit-err?] :as opts} paths]
  (let [keepers (mapv open-keeper paths)
        started (try
                  (process/process (cond-> {:cmd (vec argv)
                                            :env (environment opts)
                                            :shutdown nil
                                            :out (File. ^String (first paths))
                                            :err (if inherit-err? :inherit (File. ^String (second paths)))}
                                     dir (assoc :dir (str dir))
                                     (input-file in) (assoc :in (input-file in))))
                  (catch Exception e e))]
    (if (instance? Exception started)
      (do (run! close-quietly keepers)
          {:failure (str "could not run the program: " (reason-of started))})
      (let [^Process proc (:proc started)]
        (try
          {:proc proc :readers (mapv open-reader paths)}
          (catch Exception e
            (.destroyForcibly proc)
            (throw e))
          (finally
            (run! close-quietly keepers)))))))

(defn- stdout-sink
  "The sink for stdout, or {:failure reason} when its file cannot be
   opened."
  [{:keys [out out-keep max-out-bytes]}]
  (if out
    (try
      (file-sink (open-output-file (:file out)))
      (catch IOException e
        {:failure (str "could not open the stdout file: " (reason-of e))}))
    (capture-sink out-keep max-out-bytes)))

(defn- result
  [exit timed-out? opts out err]
  (merge {:exit exit
          :err (text (:bytes err))
          :timed-out? timed-out?
          :launched? true
          :err-truncated? (:dropped? err)}
         (if (:out opts)
           {:out nil :out-truncated? false :out-bytes (:written out) :out-error (:error out)}
           {:out (text (:bytes out)) :out-truncated? (:dropped? out)})))

(defn- run-with-pipes
  [argv {:keys [in max-err-bytes err-keep inherit-err? on-start] :as opts} deadline ^String pipe-dir]
  (let [paths (cond-> [(str pipe-dir File/separator "out")]
                (not inherit-err?) (conj (str pipe-dir File/separator "err")))
        failure (make-fifos! paths (or deadline (deadline-of setup-ms)))
        out-sink (when-not failure (stdout-sink opts))
        {:keys [^Process proc readers] :as launched} (when-not (or failure (:failure out-sink))
                                                       (launch argv opts paths))]
    (if-let [reason (or (some->> failure (str "could not create the output pipes: "))
                        (:failure out-sink)
                        (:failure launched))]
      (do (when-let [finish (:finish out-sink)] (finish))
          (launch-failure reason opts))
      (let [err-sink (if inherit-err?
                       (locked-sink (fn [_ _] true) (fn [] {:bytes (byte-array 0) :dropped? false}))
                       (capture-sink err-keep max-err-bytes))
            seen (atom #{})
            {:keys [state halt! done!]} (controller proc seen pipe-dir)
            _ (feed! (.getOutputStream proc) in)
            out-thread (drain! (first readers) out-sink)
            err-thread (if inherit-err?
                         (daemon! (fn []))
                         (drain! (second readers) err-sink))
            collect! (fn []
                       ;; The pipes close once the killed processes are
                       ;; gone; a short grace collects what they wrote
                       ;; before. A pipe a process out of reach still holds
                       ;; is abandoned to its daemon thread, whose sink no
                       ;; longer accepts bytes once finished.
                       (let [grace (deadline-of grace-ms)]
                         (.waitFor proc (max 1 (remaining-ms grace)) TimeUnit/MILLISECONDS)
                         (.join out-thread (max 1 (remaining-ms grace)))
                         (.join err-thread (max 1 (remaining-ms grace)))))
            interrupted! (fn [^InterruptedException e]
                           (halt! :interrupted)
                           ((:finish out-sink))
                           (.interrupt (Thread/currentThread))
                           (throw e))]
        (try
          (when on-start
            (try
              (on-start {:pid (.pid proc) :stop (fn [] (boolean (halt! :stopped)))})
              (catch InterruptedException e
                (interrupted! e))
              ;; Whatever the callback threw, the program must not outlive it.
              #_{:splint/disable [lint/catch-throwable]}
              (catch Throwable e
                (halt! :on-start-failed)
                (collect!)
                ((:finish out-sink))
                ((:finish err-sink))
                (throw (ex-info "the :on-start callback threw; the program was stopped"
                                {:type ::on-start-failed}
                                e)))))
          (let [outcome (try
                          (await! proc out-thread err-thread deadline seen #(= :stopped (state)))
                          (catch InterruptedException e
                            (interrupted! e)))
                timed-out? (and (= :deadline outcome) (halt! :timed-out))]
            (when-not (= :finished outcome)
              (collect!))
            (let [final (done!)
                  exit (cond
                         timed-out? 124
                         ;; A killed program not reaped within the grace.
                         (.isAlive proc) 137
                         :else (.exitValue proc))]
              (cond-> (result exit (boolean timed-out?) opts ((:finish out-sink)) ((:finish err-sink)))
                on-start (assoc :stopped? (= :stopped final)))))
          (finally
            (done!)))))))

(defn run
  "Runs argv, a non-empty sequence of strings whose first element is the
   program, with the options in `defaults`, and returns
   {:exit :out :err :timed-out? :launched? :out-truncated? :err-truncated?},
   with :out nil and :out-bytes and :out-error added when stdout goes to a
   file. A program that cannot be launched answers exit 127, one that
   overruns :timeout-ms answers exit 124 after its process tree was killed.
   A run whose :out-error is set left an incomplete file, whatever its
   exit. With :on-start the result also carries :stopped?, and a callback
   that throws makes `run` refuse with ::on-start-failed after the
   program was stopped."
  ([argv] (run argv nil))
  ([argv opts]
   (check-argv! argv)
   ;; Realized before the deadline is armed: the first call may load a
   ;; namespace.
   @native-mkfifo
   (let [opts (options opts)
         deadline (deadline-of (:timeout-ms opts))
         pipe-dir (try
                    (str (fs/real-path (fs/create-temp-dir {:prefix "briko-process-"})))
                    (catch IOException e e))]
     (if (instance? IOException pipe-dir)
       (launch-failure (str "could not create the output pipes: " (reason-of pipe-dir)) opts)
       (try
         (run-with-pipes argv opts deadline pipe-dir)
         (finally
           (try (fs/delete-tree pipe-dir) (catch IOException _ nil))))))))

;; Started programs

(def start-defaults
  "The options `start` uses where the caller gives none.

   - :extra-env, :unset-env, :dir, :in and :inherit-err? as for `run`.
   - :out, where stdout goes: nil keeps its last bytes; {:file path}
     writes it into the file at path, created owner-only or truncated,
     and refused when path is a symbolic link or not a regular file.
   - :max-out-bytes, the end of stdout kept: 64 KiB.
   - :max-err-bytes, the end of stderr kept: 64 KiB.
   - :stop-on-exit?, whether the program is stopped when the JVM shuts
     down, also on SIGTERM or SIGHUP: true. With false the caller alone
     decides when it ends, and it may outlive the JVM."
  {:extra-env {}
   :unset-env []
   :dir nil
   :in nil
   :out nil
   :max-out-bytes 65536
   :max-err-bytes 65536
   :inherit-err? false
   :stop-on-exit? true})

(def ^:private start-contradictions
  [[:out #{:max-out-bytes} "stdout written to a file is not kept"]
   [:inherit-err? #{:max-err-bytes} "inherited stderr is not kept"]])

(def wait-ready-defaults
  "The options `wait-ready` uses where the caller gives none.

   - :pattern, a regex found in a line of the program's output, or :port,
     a TCP port on the loopback interface that accepts a connection:
     exactly one of them is required.
   - :stream, the output :pattern is looked for in, :out or :err: :out.
   - :host, the loopback address :port is tried on, \"127.0.0.1\" or
     \"::1\": \"127.0.0.1\".
   - :timeout-ms, the deadline in milliseconds: 30000. There is no
     unbounded wait.
   - :settle-ms, how long the program must still run once it became
     ready: 0. A port can accept because another program bound it; a
     program that exits on its own failed bind is told apart this way.
   - :close-on-refusal?, whether a refused or interrupted wait closes
     the program: true."
  {:pattern nil
   :port nil
   :stream :out
   :host "127.0.0.1"
   :timeout-ms 30000
   :settle-ms 0
   :close-on-refusal? true})

(def ^:private wait-ready-checks
  {:pattern [(fn [x] (or (nil? x) (instance? java.util.regex.Pattern x))) "nil or a regex"]
   :port [(fn [x] (or (nil? x) (and (int? x) (<= 1 (long x) 65535)))) "nil or an int from 1 to 65535"]
   :stream [#{:out :err} ":out or :err"]
   :host [#{"127.0.0.1" "::1"} "\"127.0.0.1\" or \"::1\""]
   :timeout-ms [(fn [x] (and (int-at-most? Integer/MAX_VALUE x) (pos? (long x))))
                "a positive int of at most Integer/MAX_VALUE milliseconds"]
   :settle-ms [(partial int-at-most? Integer/MAX_VALUE) "an int from 0 to Integer/MAX_VALUE milliseconds"]
   :close-on-refusal? [boolean? "a boolean"]})

;; Lines, for ready patterns

(def ^:private max-line-bytes
  "The bytes of a line a ready pattern is matched against; the rest of a
   longer line is not looked at."
  4096)

(def ^:private history-lines
  "How many of a stream's latest lines a ready pattern is matched against
   besides the lines that arrive while it waits."
  64)

(defn- line-log
  []
  {:lock (Object.)
   :partial (ByteArrayOutputStream.)
   :history (volatile! [])
   :watchers (volatile! #{})})

(defn- emit-line!
  "Ends the partial line: keeps it in the history and hands it to every
   watcher. Called under the log's lock."
  [{:keys [^ByteArrayOutputStream partial history watchers]}]
  (let [s (String. (.toByteArray partial) StandardCharsets/UTF_8)
        line (if (str/ends-with? s "\r") (subs s 0 (dec (count s))) s)]
    (.reset partial)
    (vswap! history (fn [h] (let [h (conj h line)] (if (> (count h) history-lines) (vec (rest h)) h))))
    (run! (fn [w] (w line)) @watchers)))

(defn- feed-lines!
  "Splits the n bytes of buf into lines. The bytes are searched for
   newlines through an ISO-8859-1 string, one char per byte, so the search
   is native on both runtimes; each line is decoded as UTF-8."
  [{:keys [lock ^ByteArrayOutputStream partial] :as log} ^bytes buf n]
  (let [n (long n)
        s (String. buf 0 (int n) StandardCharsets/ISO_8859_1)
        append! (fn [from to]
                  (let [k (min (- (long to) (long from)) (max 0 (- (long max-line-bytes) (.size partial))))]
                    (when (pos? k) (.write partial buf (int from) (int k)))))]
    (locking lock
      (loop [from 0]
        (let [i (.indexOf s "\n" (int from))]
          (if (neg? i)
            (append! from n)
            (do (append! from i)
                (emit-line! log)
                (recur (inc i)))))))))

(defn- flush-line!
  "Ends a last line without a newline at end of file."
  [{:keys [lock ^ByteArrayOutputStream partial] :as log}]
  (locking lock
    (when (pos? (.size partial))
      (emit-line! log))))

(defn- watched
  "sink, with every chunk also split into lines on log."
  [sink log]
  (assoc sink
         :consume (fn [buf n] (feed-lines! log buf n) ((:consume sink) buf n))
         :eof (fn [] (flush-line! log))))

(defn- watch!
  "Delivers matched once a line of log matches pattern, looking at the
   history first. Returns a function that stops watching."
  [{:keys [lock history watchers]} pattern matched]
  (let [w (fn [line] (when (re-find pattern line) (deliver matched true)))]
    (locking lock
      (if (some (fn [line] (re-find pattern line)) @history)
        (deliver matched true)
        (vswap! watchers conj w)))
    (fn [] (locking lock (vswap! watchers disj w)))))

;; The registry of started programs

(defonce ^:private registry
  ;; id -> handle of every started program with :stop-on-exit? true that
  ;; has neither been closed nor finished on its own.
  (atom {}))

(declare close)

(defn- stop-all!
  "Stops every registered program: kills them all first, so none waits
   for another's grace, then closes each. What the shutdown hook runs."
  []
  (let [handles (vals @registry)]
    (doseq [h handles]
      #_{:splint/disable [lint/catch-throwable]}
      (try ((::kill! h)) (catch Throwable _ nil)))
    (doseq [h handles]
      #_{:splint/disable [lint/catch-throwable]}
      (try (close h) (catch Throwable _ nil)))))

(defonce ^:private shutdown-hook
  ;; One hook for every started program, registered on the first start
  ;; that asks for it. SIGTERM and SIGHUP run shutdown hooks and halt the
  ;; JVM without unwinding any thread, so no finally of the caller runs.
  ;; The hook is a registered, unstarted Thread; it holds no reference
  ;; that keeps the JVM running, and stop-all! is bounded by the kills and
  ;; one grace per program.
  (delay (let [t (Thread. ^Runnable (fn [] (stop-all!)) "briko.process shutdown")]
           (.addShutdownHook (Runtime/getRuntime) t)
           t)))

;; Starting

(defn- start-with-pipes
  "Launches argv with its output on named pipes below pipe-dir. Returns
   the parts of a started program, or {:failure reason}."
  [argv {:keys [in inherit-err? max-err-bytes] :as opts} ^String pipe-dir]
  (let [paths (cond-> [(str pipe-dir File/separator "out")]
                (not inherit-err?) (conj (str pipe-dir File/separator "err")))
        failure (make-fifos! paths (deadline-of setup-ms))
        out-sink (when-not failure (stdout-sink (assoc opts :out-keep :tail)))
        launched (when-not (or failure (:failure out-sink))
                   (launch argv opts paths))]
    (if-let [reason (or (some->> failure (str "could not create the output pipes: "))
                        (:failure out-sink)
                        (:failure launched))]
      (do (when-let [finish (:finish out-sink)] (finish))
          {:failure reason})
      (let [^Process proc (:proc launched)
            readers (:readers launched)
            nothing (fn [] {:bytes (byte-array 0) :dropped? false})
            err-sink (if inherit-err?
                       (locked-sink (fn [_ _] true) nothing nothing)
                       (tail-sink max-err-bytes))
            out-log (line-log)
            err-log (line-log)
            out-sink (watched out-sink out-log)
            err-sink (watched err-sink err-log)]
        (feed! (.getOutputStream proc) in)
        {:proc proc
         :out-sink out-sink
         :err-sink err-sink
         :out-log out-log
         :err-log err-log
         :out-thread (drain! (first readers) out-sink)
         :err-thread (if inherit-err?
                       (daemon! (fn []))
                       (drain! (second readers) err-sink))}))))

(defn- output-of
  [opts out err]
  (dissoc (result 0 false opts out err) :exit :timed-out? :launched?))

(defn- failed-handle
  "The handle of a program that did not start: never alive, closed with
   127, its reason in the stderr of `output`."
  [reason opts]
  (let [failure (launch-failure reason opts)]
    {:pid nil
     :launched? false
     ::alive? (constantly false)
     ::exit-value (constantly 127)
     ::output (constantly (dissoc failure :exit :timed-out? :launched?))
     ::watch (fn [_ _ _] (fn []))
     ::kill! (constantly nil)
     ::close! (constantly 127)}))

(defn- started-handle
  "The handle of a started program. One lock orders its states:
   :running becomes :finished when the program exited and its output
   closed, or :killed when close or the shutdown hook killed it; both end
   in :closed once close collected what was left."
  [{:keys [^Process proc out-sink err-sink out-log err-log ^Thread out-thread ^Thread err-thread]}
   {:keys [stop-on-exit?] :as opts} pipe-dir]
  (let [lock (Object.)
        state (volatile! :running)
        exit (volatile! nil)
        seen (atom #{})
        id (Object.)
        finish-sinks! (fn [] ((:finish out-sink)) ((:finish err-sink)))
        unregister! (fn [] (swap! registry dissoc id))
        kill! (fn []
                (locking lock
                  (when (= :running @state)
                    (vreset! state :killed)
                    (kill-all! proc seen pipe-dir))))
        close! (fn []
                 (locking lock
                   (when (= :running @state)
                     (vreset! state :killed)
                     (kill-all! proc seen pipe-dir))
                   (when (= :killed @state)
                     ;; The pipes close once the killed processes are
                     ;; gone; a pipe a process out of reach still holds
                     ;; is abandoned to its daemon thread, whose sink no
                     ;; longer accepts bytes once finished.
                     (let [grace (deadline-of grace-ms)]
                       (.waitFor proc (max 1 (remaining-ms grace)) TimeUnit/MILLISECONDS)
                       (.join out-thread (max 1 (remaining-ms grace)))
                       (.join err-thread (max 1 (remaining-ms grace))))
                     (vreset! exit (if (.isAlive proc) 137 (.exitValue proc)))
                     (finish-sinks!)
                     (unregister!))
                   (vreset! state :closed)
                   @exit))
        monitor (fn []
                  (try
                    ;; Descendants are sampled while the program runs, so
                    ;; a kill reaches children it detached since.
                    (loop [interval 10]
                      (when-not (.waitFor proc interval TimeUnit/MILLISECONDS)
                        (sample! proc seen)
                        (recur (min 1000 (* 2 interval)))))
                    (.join out-thread)
                    (.join err-thread)
                    (locking lock
                      (when (= :running @state)
                        (vreset! exit (.exitValue proc))
                        (finish-sinks!)
                        (vreset! state :finished)
                        (unregister!)))
                    (catch InterruptedException _ nil)))
        handle {:pid (.pid proc)
                :launched? true
                ::opts opts
                ::alive? (fn [] (.isAlive proc))
                ::exit-value (fn [] (when-not (.isAlive proc) (.exitValue proc)))
                ::output (fn [] (output-of opts ((:peek out-sink)) ((:peek err-sink))))
                ::watch (fn [stream pattern matched]
                          (watch! (if (= :err stream) err-log out-log) pattern matched))
                ::kill! kill!
                ::close! close!}]
    ;; Registered before the monitor starts, so a program that finishes at
    ;; once is unregistered after it was registered, never before.
    (when stop-on-exit?
      @shutdown-hook
      (swap! registry assoc id handle))
    (daemon! monitor)
    handle))

(defn start
  "Starts argv, a non-empty sequence of strings whose first element is
   the program, to run alongside the caller, with the options in
   `start-defaults`, and returns a handle {:pid :launched?} for `alive?`,
   `output`, `wait-ready` and `close`.

   The program runs without a deadline until it exits or `close` ends it;
   with :stop-on-exit? true, the default, it is also stopped when the JVM
   shuts down. Its output is read all the time, so it never blocks on a
   full pipe, and only the last :max-out-bytes and :max-err-bytes are
   kept. A program that cannot be launched, also for want of `mkfifo`,
   gives a handle with :pid nil and :launched? false that is never alive,
   whose `output` carries the reason in :err and whose `close` answers
   127. Invalid arguments are refused as for `run`."
  ([argv] (start argv nil))
  ([argv opts]
   (check-argv! argv)
   (let [opts (checked-options opts start-defaults start-contradictions)
         pipe-dir (try
                    (str (fs/real-path (fs/create-temp-dir {:prefix "briko-process-"})))
                    (catch IOException e e))]
     (if (instance? IOException pipe-dir)
       (failed-handle (str "could not create the output pipes: " (reason-of pipe-dir)) opts)
       (let [parts (try
                     (start-with-pipes argv opts pipe-dir)
                     (finally
                       ;; Both ends of every pipe are open now, so the
                       ;; names can go: nothing is left on disk whatever
                       ;; becomes of this JVM. /proc still names a removed
                       ;; pipe by its old path, which the holder kill
                       ;; relies on.
                       (try (fs/delete-tree pipe-dir) (catch IOException _ nil))))]
         (if-let [reason (:failure parts)]
           (failed-handle reason opts)
           (started-handle parts opts pipe-dir)))))))

(defn- check-handle!
  [h]
  (when-not (and (map? h) (fn? (::close! h)))
    (refuse! ::invalid-handle "not a handle returned by start" {})))

(defn alive?
  "Whether the started program is running."
  [h]
  (check-handle! h)
  (boolean ((::alive? h))))

(defn output
  "What the started program wrote so far, kept as `start` was told:
   {:out :err :out-truncated? :err-truncated?}, with :out nil and
   :out-bytes and :out-error instead when stdout goes to a file. A set
   :out-error means the file is incomplete. The truncated flags say the
   stream was longer than its limit, so its start is gone."
  [h]
  (check-handle! h)
  ((::output h)))

(defn close
  "Ends the started program: kills it, every descendant seen while it
   ran and every process holding its output, waits a short grace for its
   last output, and returns its exit: 137 after SIGKILL, its own exit when
   it had exited before, 127 when it never started. Idempotent: a later
   call returns the same exit and kills nothing."
  [h]
  (check-handle! h)
  ((::close! h)))

(defn- wait-options
  [opts h]
  (let [given opts
        _ (when-not (or (nil? opts) (map? opts))
            (refuse! ::invalid-options "the options must be a map" {}))
        _ (when-let [unknown (seq (remove (set (keys wait-ready-defaults)) (keys opts)))]
            (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
        opts (merge wait-ready-defaults opts)]
    (doseq [[k [valid? wanted]] wait-ready-checks]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k})))
    (when-not (= 1 (count (filter some? [(:pattern opts) (:port opts)])))
      (refuse! ::invalid-options "exactly one of :pattern and :port must be given" {:option :pattern}))
    (when (and (:pattern opts) (contains? given :host))
      (refuse! ::invalid-options ":pattern rules out [:host]: no port is tried" {:option :host}))
    (when (and (:port opts) (contains? given :stream))
      (refuse! ::invalid-options ":port rules out [:stream]: no output is watched" {:option :stream}))
    (when (and (:pattern opts) (= :err (:stream opts)) (:launched? h) (:inherit-err? (::opts h)))
      (refuse! ::invalid-options "stderr is inherited and cannot be watched" {:option :stream}))
    opts))

(defn- port-open?
  [^String host port timeout-ms]
  (try
    (with-open [socket (java.net.Socket.)]
      (.connect socket (java.net.InetSocketAddress. host (int port)) (int timeout-ms))
      true)
    (catch IOException _ false)))

(defn wait-ready
  "Waits until the started program is ready, with the options in
   `wait-ready-defaults`, and returns the handle: until a line of its
   output matches :pattern, or :port on the loopback interface accepts a
   connection, and the program still runs :settle-ms later.

   A pattern is matched against the lines that arrive while it waits and
   the latest lines before, up to 64 per stream, each up to its first
   4096 bytes; call it soon after `start`, before a chatty program pushes
   its ready line out.

   Refuses with ::exited-before-ready, whose data carries :exit, when the
   program is not running, and with ::ready-timeout when the deadline
   passes; with :close-on-refusal? true, the default, the program is
   closed first. Neither message nor data carries the program's output;
   `output` still holds it. An interrupted wait closes the program the
   same way and rethrows the InterruptedException with the flag set."
  ([h opts]
   (check-handle! h)
   (let [{:keys [pattern port stream host timeout-ms settle-ms close-on-refusal?]} (wait-options opts h)
         deadline (deadline-of timeout-ms)
         settle-ns (* 1000000 (long settle-ms))
         matched (promise)
         unwatch (when pattern ((::watch h) stream pattern matched))
         ready? (fn []
                  (if pattern
                    (realized? matched)
                    (port-open? host port (max 1 (min 200 (remaining-ms deadline))))))]
     (try
       (loop [ready-since nil]
         (let [ready-since (or ready-since (when (ready?) (System/nanoTime)))]
           (cond
             (not ((::alive? h)))
             (let [exit (if close-on-refusal? (close h) ((::exit-value h)))]
               (refuse! ::exited-before-ready "the program is not running and was not ready" {:exit exit}))

             (and ready-since (>= (- (System/nanoTime) (long ready-since)) settle-ns))
             h

             (zero? (remaining-ms deadline))
             (do (when close-on-refusal? (close h))
                 (refuse! ::ready-timeout (str "the program was not ready within " timeout-ms " ms")
                          {:timeout-ms timeout-ms}))

             :else
             (let [step (max 1 (min 50 (remaining-ms deadline)))]
               (if (and pattern (not ready-since))
                 (deref matched step nil)
                 (Thread/sleep step))
               (recur ready-since)))))
       (catch InterruptedException e
         (when close-on-refusal? (close h))
         (.interrupt (Thread/currentThread))
         (throw e))
       (finally
         (when unwatch (unwatch)))))))
