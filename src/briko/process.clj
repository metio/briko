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
   with :launched? false, as for a program that cannot be launched.

   Stdout written to a file ({:out {:file path}}) travels through the same
   pipe and is copied into the file by this process, so a detached child
   holding stdout makes the run 124 exactly as when stdout is captured,
   and nothing reaches the file after `run` returned.

   With {:timeout-ms :none} the run has no deadline: it lasts until the
   program and every holder of its output are done, or until the waiting
   thread is interrupted, which kills the tree as a timeout would.

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
     of being captured: false."
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
   :inherit-err? false})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-argv ::invalid-options})

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
   :inherit-err? [boolean? "a boolean"]})

(def ^:private contradictions
  "[option given, options it rules out, why], each refused when given
   together, because the second would be silently ignored."
  [[:out #{:max-out-bytes :out-keep} "stdout written to a file is not captured"]
   [:inherit-err? #{:max-err-bytes :err-keep} "inherited stderr is not captured"]])

(defn- options
  [opts]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [given opts
        opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks]
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
  [consume finish]
  (let [lock (Object.)
        open (volatile! true)]
    {:consume (fn [buf n] (locking lock (and @open (consume buf n))))
     :finish (fn [] (locking lock (vreset! open false) (finish)))}))

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
        total (volatile! 0)]
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
     (fn []
       (let [dropped? (> (long @total) limit)
             kept (last-bytes (.toByteArray sink) limit)]
         {:bytes (if dropped? (from-whole-character kept) kept)
          :dropped? dropped?})))))

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
       {:written @written :error @error}))))

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
   sink's consume, until it answers false."
  ^Thread [^InputStream in {:keys [consume]}]
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
           (try (.close in) (catch IOException _ nil))))))))

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

(defn- make-fifos!
  "Creates a named pipe, readable and writable by the owner only, at each
   of paths with mkfifo, within the deadline. Returns nil, or the reason
   it could not."
  [paths deadline]
  (try
    (let [proc (.start (doto (ProcessBuilder. ^List (into ["mkfifo" "-m" "600"] paths))
                         (.redirectErrorStream true)))]
      (.close (.getOutputStream proc))
      (if (.waitFor proc (max 1 (remaining-ms deadline)) TimeUnit/MILLISECONDS)
        (when-not (zero? (.exitValue proc))
          (str "mkfifo failed: " (str/trim (String. (.readAllBytes (.getInputStream proc)) StandardCharsets/UTF_8))))
        (do (.destroyForcibly proc)
            "mkfifo overran the deadline")))
    (catch IOException e
      (str "mkfifo could not run: " (ex-message e)))))

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

(defn- descendants-of
  [^Process proc]
  (iterator-seq (.iterator (.descendants (.toHandle proc)))))

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

(def ^:private grace-ms
  "How long a timed-out run waits for the output its killed tree wrote."
  200)

(defn- await!
  "Waits until the program has exited and both output threads finished, or
   the deadline passed, sampling descendants with a growing interval while
   the program runs. Returns true when the run finished in time.

   The first sample is taken only after the program ran for one interval:
   walking the process table costs about 15 ms with 600 processes, more
   than a short program takes, and the samples serve only to kill a tree
   that outlives its deadline, which a program that has exited cannot."
  [^Process proc ^Thread out-thread ^Thread err-thread deadline seen]
  (loop [interval 10]
    (let [left (remaining-ms deadline)
          step (min left (long interval))]
      (cond
        (and (not (.isAlive proc)) (not (.isAlive out-thread)) (not (.isAlive err-thread)))
        true

        (zero? left)
        false

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
  [argv {:keys [in max-err-bytes err-keep inherit-err?] :as opts} deadline ^String pipe-dir]
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
            _ (feed! (.getOutputStream proc) in)
            out-thread (drain! (first readers) out-sink)
            err-thread (if inherit-err?
                         (daemon! (fn []))
                         (drain! (second readers) err-sink))
            finished? (try
                        (await! proc out-thread err-thread deadline seen)
                        (catch InterruptedException e
                          (kill-tree! proc seen)
                          (kill-holders! pipe-dir)
                          ((:finish out-sink))
                          (.interrupt (Thread/currentThread))
                          (throw e)))]
        (when-not finished?
          (kill-tree! proc seen)
          (kill-holders! pipe-dir)
          ;; The pipes close once the killed processes are gone; a short
          ;; grace collects what they wrote before the deadline. A pipe a
          ;; process out of reach still holds is abandoned to its daemon
          ;; thread, whose sink no longer accepts bytes once finished.
          (let [grace (deadline-of grace-ms)]
            (.join out-thread (max 1 (remaining-ms grace)))
            (.join err-thread (max 1 (remaining-ms grace)))))
        (result (if finished? (.exitValue proc) 124) (not finished?) opts
                ((:finish out-sink)) ((:finish err-sink)))))))

(defn run
  "Runs argv, a non-empty sequence of strings whose first element is the
   program, with the options in `defaults`, and returns
   {:exit :out :err :timed-out? :launched? :out-truncated? :err-truncated?},
   with :out nil and :out-bytes and :out-error added when stdout goes to a
   file. A program that cannot be launched answers exit 127, one that
   overruns :timeout-ms answers exit 124 after its process tree was killed.
   A run whose :out-error is set left an incomplete file, whatever its
   exit."
  ([argv] (run argv nil))
  ([argv opts]
   (check-argv! argv)
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
