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
   the program has exited and both of its output streams are closed, so a
   background child that keeps stdout open cannot stall the caller past
   the deadline. Descendants are sampled while the program runs, densely
   at first and then every 500 ms, and on a timeout every sampled
   descendant still alive is killed, also those whose parent already
   exited. A descendant that is started and detached between two samples,
   such as the background job of a shell that exits at once, is out of
   reach, because Java cannot put the program in a process group of its
   own. Such a run still answers at the deadline, 124 when the detached
   process holds the output open, and leaves that process running.

   Output is captured up to a byte limit per stream. Past the limit the
   stream is still read, so the program never blocks on a full pipe, but
   the excess is discarded and :out-truncated? or :err-truncated? is set.
   Output is decoded as UTF-8, replacing malformed bytes.

   Invalid arguments are a programming error and throw an ex-info whose
   :type is in `refusal-types`; an unknown option is refused, so a
   misspelt :timeout-ms cannot fall back to the default unnoticed. A
   thread interrupted while it waits kills the tree and rethrows the
   InterruptedException with its interrupt flag set."
  (:require [babashka.process :as process]
            [clojure.string :as str])
  (:import (java.io ByteArrayOutputStream File IOException InputStream OutputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Path)
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

(defn- remaining-ms
  ^long [^long deadline]
  (max 0 (quot (- deadline (System/nanoTime)) 1000000)))

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
  [^Exception e]
  {:exit 127
   :out ""
   :err (str "could not run the program: " (or (ex-message e) (.getName (class e))))
   :timed-out? false
   :launched? false
   :out-truncated? false
   :err-truncated? false})

(defn run
  "Runs argv, a non-empty sequence of strings whose first element is the
   program, with the options in `defaults`, and returns
   {:exit :out :err :timed-out? :launched? :out-truncated? :err-truncated?}.
   A program that cannot be launched answers exit 127, one that overruns
   :timeout-ms answers exit 124 after its process tree was killed."
  ([argv] (run argv nil))
  ([argv opts]
   (check-argv! argv)
   (let [{:keys [timeout-ms dir in max-out-bytes max-err-bytes inherit-err?] :as opts} (options opts)
         deadline (+ (System/nanoTime) (* 1000000 (long timeout-ms)))
         started (try
                   (process/process (cond-> {:cmd (vec argv)
                                             :env (environment opts)
                                             :shutdown nil}
                                      dir (assoc :dir (str dir))
                                      inherit-err? (assoc :err :inherit)))
                   (catch Exception e e))]
     (if (instance? Exception started)
       (launch-failure started)
       (let [^Process proc (:proc started)
             out-sink (ByteArrayOutputStream.)
             err-sink (ByteArrayOutputStream.)
             out-dropped (volatile! false)
             err-dropped (volatile! false)
             seen (atom #{})
             _ (feed! (.getOutputStream proc) in)
             out-thread (drain! (.getInputStream proc) out-sink max-out-bytes out-dropped)
             err-thread (if inherit-err?
                          (daemon! (fn []))
                          (drain! (.getErrorStream proc) err-sink max-err-bytes err-dropped))
             finished? (try
                         (await! proc out-thread err-thread deadline seen)
                         (catch InterruptedException e
                           (kill-tree! proc seen)
                           (.interrupt (Thread/currentThread))
                           (throw e)))]
         (when-not finished?
           (kill-tree! proc seen)
           ;; The pipes close once the killed processes are gone; a short
           ;; grace collects what they wrote before the deadline. A pipe a
           ;; detached process still holds is abandoned to its daemon
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
          :err-truncated? @err-dropped})))))
