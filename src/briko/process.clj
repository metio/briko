;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process
  "Runs a program as an argv, never through a shell, and always comes back
   with an answer within a deadline.

   `run` returns {:exit :out :err :timed-out? :launched? :out-truncated?
   :err-truncated?}. It does not throw when the program cannot be started
   or overruns its deadline:

     - a program that cannot be launched (missing, not executable, a
       working directory that does not exist) answers exit 127, the shell's
       convention for 'command not found', with the reason in :err and
       :launched? false;
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

   On a timeout the program is killed with SIGKILL, then every descendant
   seen while it ran, then, where /proc exists, every process still
   holding one of its output pipes, also one started and detached between
   two samples. Descendants are sampled while the program runs, densely
   at first and then every 500 ms. A descendant that was detached between
   two samples and does not hold the output is out of reach, because Java
   cannot put the program in a process group of its own; it neither
   delays the run nor is killed by it.

   Output is captured up to a byte limit per stream. Past the limit the
   stream is still read, so the program never blocks on a full pipe, but
   the excess is discarded and :out-truncated? or :err-truncated? is set.
   Output is decoded as UTF-8, replacing malformed bytes.

   Invalid arguments are a programming error and throw an ex-info whose
   :type is in `refusal-types`; an unknown option is refused, so a
   misspelt :timeout-ms cannot fall back to the default unnoticed. A
   thread interrupted while it waits kills the tree and rethrows the
   InterruptedException with its interrupt flag set."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str])
  (:import (java.io ByteArrayOutputStream File FileInputStream IOException InputStream OutputStream
                    RandomAccessFile)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)
           (java.util List)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a run uses where the caller gives none.

   - :timeout-ms, the deadline from launch in milliseconds: 60000. Long
     enough for a password manager waiting on a hardware key, short enough
     that a wedged tool cannot hold a scheduled run for minutes.
   - :extra-env, variables added to the inherited environment: none.
   - :unset-env, variables removed from it: none.
   - :dir, the working directory: the current one.
   - :in, a string or byte array written to stdin, which is then closed:
     nothing, so stdin is closed at once and a program waiting for input
     reads end of file instead of hanging.
   - :max-out-bytes, the stdout captured: 1 MiB.
   - :max-err-bytes, the stderr captured: 64 KiB.
   - :inherit-err?, whether stderr goes to this process's stderr instead
     of being captured: false."
  {:timeout-ms 60000
   :extra-env {}
   :unset-env []
   :dir nil
   :in nil
   :max-out-bytes 1048576
   :max-err-bytes 65536
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

(def ^:private option-checks
  {:timeout-ms [(fn [x] (and (int-at-most? Integer/MAX_VALUE x) (pos? (long x))))
                "a positive int of at most Integer/MAX_VALUE milliseconds"]
   :extra-env [(fn [m] (and (map? m) (every? (fn [[k v]] (and (env-name? k) (string? v) (no-nul? v))) m)))
               "a map from variable name to string value"]
   :unset-env [(fn [xs] (and (coll? xs) (not (map? xs)) (every? env-name? xs)))
               "a collection of variable names"]
   :dir [(fn [d] (or (nil? d) (string? d) (instance? File d) (instance? Path d)))
         "nil, a string, a File or a Path"]
   :in [(fn [x] (or (nil? x) (string? x) (bytes? x))) "nil, a string or a byte array"]
   :max-out-bytes [(partial int-at-most? (dec Integer/MAX_VALUE)) "an int from 0 below Integer/MAX_VALUE"]
   :max-err-bytes [(partial int-at-most? (dec Integer/MAX_VALUE)) "an int from 0 below Integer/MAX_VALUE"]
   :inherit-err? [boolean? "a boolean"]})

(defn- options
  [opts]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k})))
    (when-let [both (seq (filter (set (:unset-env opts)) (keys (:extra-env opts))))]
      (refuse! ::invalid-options (str "variables both set and unset: " (pr-str (vec both))) {:option :unset-env}))
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

;; Streams

(defn- daemon!
  "Starts f on a daemon thread, so a thread blocked on a pipe that some
   detached grandchild holds open never keeps the JVM from exiting."
  ^Thread [f]
  (doto (Thread. ^Runnable f)
    (.setDaemon true)
    (.start)))

(defn- drain!
  "Reads in to its end on a daemon thread, keeping at most limit bytes in
   sink and counting what it drops in dropped."
  ^Thread [^InputStream in ^ByteArrayOutputStream sink limit dropped]
  (daemon!
   (fn []
     (let [buf (byte-array 8192)
           limit (long limit)]
       (try
         (loop []
           (let [n (.read in buf)]
             (when (pos? n)
               (let [room (max 0 (- limit (.size sink)))
                     kept (min room (long n))]
                 (when (pos? kept) (.write sink buf 0 (int kept)))
                 (when (< kept n) (vreset! dropped true)))
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
       (when input
         (.write stdin (if (string? input) (.getBytes ^String input StandardCharsets/UTF_8) ^bytes input))
         (.flush stdin))
       (catch IOException _ nil)
       (finally
         (try (.close stdin) (catch IOException _ nil)))))))

(defn- remaining-ms
  ^long [^long deadline]
  (max 0 (quot (- deadline (System/nanoTime)) 1000000)))

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
   the deadline passed, sampling descendants with a growing interval.
   Returns true when the run finished in time."
  [^Process proc ^Thread out-thread ^Thread err-thread deadline seen]
  (loop [interval 10]
    (sample! proc seen)
    (let [left (remaining-ms deadline)
          step (min left (long interval))]
      (cond
        (and (not (.isAlive proc)) (not (.isAlive out-thread)) (not (.isAlive err-thread)))
        true

        (zero? left)
        false

        :else
        (do (if (.isAlive proc)
              (.waitFor proc step TimeUnit/MILLISECONDS)
              (.join (if (.isAlive out-thread) out-thread err-thread) step))
            (recur (min 500 (* 2 interval))))))))

(defn- text
  ^String [^ByteArrayOutputStream sink]
  (String. (.toByteArray sink) StandardCharsets/UTF_8))

(defn- launch-failure
  [^String reason]
  {:exit 127
   :out ""
   :err reason
   :timed-out? false
   :launched? false
   :out-truncated? false
   :err-truncated? false})

(defn- reason-of
  ^String [^Exception e]
  (or (ex-message e) (.getName (class e))))

(defn- launch
  "Starts the program with stdout, and stderr unless it is inherited,
   redirected to the named pipes at paths, and opens their read ends.
   Returns {:proc :readers}, or {:failure reason} when it did not start."
  [argv {:keys [dir inherit-err?] :as opts} paths]
  (let [keepers (mapv open-keeper paths)
        started (try
                  (process/process (cond-> {:cmd (vec argv)
                                            :env (environment opts)
                                            :shutdown nil
                                            :out (File. ^String (first paths))
                                            :err (if inherit-err? :inherit (File. ^String (second paths)))}
                                     dir (assoc :dir (str dir))))
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

(defn- run-with-pipes
  [argv {:keys [in max-out-bytes max-err-bytes inherit-err?] :as opts} deadline ^String pipe-dir]
  (let [paths (cond-> [(str pipe-dir File/separator "out")]
                (not inherit-err?) (conj (str pipe-dir File/separator "err")))
        failure (make-fifos! paths deadline)
        {:keys [^Process proc readers] :as launched} (when-not failure (launch argv opts paths))]
    (if-let [reason (or (some->> failure (str "could not create the output pipes: ")) (:failure launched))]
      (launch-failure reason)
      (let [out-sink (ByteArrayOutputStream.)
            err-sink (ByteArrayOutputStream.)
            out-dropped (volatile! false)
            err-dropped (volatile! false)
            seen (atom #{})
            _ (feed! (.getOutputStream proc) in)
            out-thread (drain! (first readers) out-sink max-out-bytes out-dropped)
            err-thread (if inherit-err?
                         (daemon! (fn []))
                         (drain! (second readers) err-sink max-err-bytes err-dropped))
            finished? (try
                        (await! proc out-thread err-thread deadline seen)
                        (catch InterruptedException e
                          (kill-tree! proc seen)
                          (kill-holders! pipe-dir)
                          (.interrupt (Thread/currentThread))
                          (throw e)))]
        (when-not finished?
          (kill-tree! proc seen)
          (kill-holders! pipe-dir)
          ;; The pipes close once the killed processes are gone; a short
          ;; grace collects what they wrote before the deadline. A pipe a
          ;; process out of reach still holds is abandoned to its daemon
          ;; thread.
          (let [grace (+ (System/nanoTime) (* 1000000 grace-ms))]
            (.join out-thread (max 1 (remaining-ms grace)))
            (.join err-thread (max 1 (remaining-ms grace)))))
        {:exit (if finished? (.exitValue proc) 124)
         :out (text out-sink)
         :err (text err-sink)
         :timed-out? (not finished?)
         :launched? true
         :out-truncated? @out-dropped
         :err-truncated? @err-dropped}))))

(defn run
  "Runs argv, a non-empty sequence of strings whose first element is the
   program, with the options in `defaults`, and returns
   {:exit :out :err :timed-out? :launched? :out-truncated? :err-truncated?}.
   A program that cannot be launched answers exit 127, one that overruns
   :timeout-ms answers exit 124 after its process tree was killed."
  ([argv] (run argv nil))
  ([argv opts]
   (check-argv! argv)
   (let [opts (options opts)
         deadline (+ (System/nanoTime) (* 1000000 (long (:timeout-ms opts))))
         pipe-dir (try
                    (str (fs/real-path (fs/create-temp-dir {:prefix "briko-process-"})))
                    (catch IOException e e))]
     (if (instance? IOException pipe-dir)
       (launch-failure (str "could not create the output pipes: " (reason-of pipe-dir)))
       (try
         (run-with-pipes argv opts deadline pipe-dir)
         (finally
           (try (fs/delete-tree pipe-dir) (catch IOException _ nil))))))))
