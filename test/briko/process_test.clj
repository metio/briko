;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process-test
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.util Optional)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (process/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- elapsed-ms
  "[result ms] of calling f."
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(defn- alive?
  [pid]
  (let [^Optional handle (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent handle) (.isAlive ^java.lang.ProcessHandle (.get handle)))))

(defn- gone-within?
  "Whether the process pid has gone within ms, polling."
  [pid ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (not (alive? pid)) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 20) (recur))))))

(defn- kill!
  [pid]
  (let [^Optional handle (java.lang.ProcessHandle/of (long pid))]
    (when (.isPresent handle)
      (.destroyForcibly ^java.lang.ProcessHandle (.get handle)))))

(defn- read-pid
  "The pid written to file, waiting for it to appear."
  [file]
  (loop [n 0]
    (let [s (when (fs/exists? file) (str/trim (slurp (str file))))]
      (cond
        (not (str/blank? s)) (parse-long s)
        (< n 250) (do (Thread/sleep 20) (recur (inc n)))
        :else (throw (ex-info "no pid written" {:file (str file)}))))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (fs/create-temp-dir {:prefix "briko-process-"})]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(deftest runs-an-argv-and-captures-its-result
  (is (= {:exit 3 :out "out\n" :err "err\n" :timed-out? false :launched? true
          :out-truncated? false :err-truncated? false}
         (process/run ["sh" "-c" "echo out; echo err >&2; exit 3"])))
  (is (zero? (:exit (process/run ["true"]))))
  (is (= 1 (:exit (process/run ["false"])))))

(deftest runs-without-a-shell
  (is (= "$HOME ; `id` *\n" (:out (process/run ["echo" "$HOME" ";" "`id`" "*"])))))

(deftest a-failed-launch-answers-127
  (testing "a missing program"
    (let [r (process/run ["briko-no-such-program-0f3a"])]
      (is (= 127 (:exit r)))
      (is (false? (:launched? r)))
      (is (false? (:timed-out? r)))
      (is (str/includes? (:err r) "briko-no-such-program-0f3a"))))
  (testing "a file that is not executable"
    (with-temp-dir [dir]
      (let [f (fs/path dir "script")]
        (spit (str f) "#!/bin/sh\necho hi\n")
        (is (= 127 (:exit (process/run [(str f)])))))))
  (testing "a working directory that does not exist"
    (with-temp-dir [dir]
      (is (= 127 (:exit (process/run ["pwd"] {:dir (str (fs/path dir "missing"))})))))))

(deftest a-run-past-its-deadline-answers-124
  (let [[r ms] (elapsed-ms #(process/run ["sleep" "30"] {:timeout-ms 300}))]
    (is (= 124 (:exit r)))
    (is (true? (:timed-out? r)))
    (is (true? (:launched? r)))
    (is (< ms 5000) (str "took " ms " ms"))))

(deftest a-timeout-kills-the-whole-process-tree
  (testing "a child and a grandchild still attached at the deadline"
    (with-temp-dir [dir]
      (let [child (fs/path dir "child")
            grandchild (fs/path dir "grandchild")
            script (str "sleep 300 & echo $! > " child "; "
                        "sh -c 'sleep 300 & echo $! > " grandchild "; wait' & "
                        "wait")
            r (process/run ["sh" "-c" script] {:timeout-ms 1000})
            pids [(read-pid child) (read-pid grandchild)]]
        (try
          (is (= 124 (:exit r)))
          (doseq [pid pids]
            (is (gone-within? pid 5000) (str "process " pid " survived the timeout")))
          (finally (run! kill! pids)))))))

(defn- detached-holder
  "Runs a shell that starts `sleep 300` in the background, holding the
   output named by redirect, writes its pid and exits at once. Returns
   [result elapsed-ms pid]."
  ([dir] (detached-holder dir "" {}))
  ([dir redirect opts]
   (let [pid-file (fs/path dir (str "pid-" (random-uuid)))
         [r ms] (elapsed-ms #(process/run ["sh" "-c" (str "sleep 300 " redirect " & echo $! > " pid-file)]
                                          (merge {:timeout-ms 500} opts)))]
     [r ms (read-pid pid-file)])))

(defn- with-delayed-threads
  "Calls f with every thread of briko.process starting delay-ms late, so
   the program exits before its output is read."
  [delay-ms f]
  (let [daemon! @#'briko.process/daemon!]
    (with-redefs-fn {#'briko.process/daemon! (fn [g] (daemon! (fn [] (Thread/sleep (long (delay-ms))) (g))))}
      f)))

(deftest a-detached-child-holding-the-output-cannot-stall-the-caller
  (with-temp-dir [dir]
    (doseq [[what redirect opts] [["stdout" "" {}]
                                  ["stderr" ">/dev/null" {}]
                                  ["stdout with stderr inherited" "2>/dev/null" {:inherit-err? true}]]]
      (testing what
        (let [[r ms pid] (detached-holder dir redirect opts)]
          (try
            (is (= 124 (:exit r)))
            (is (true? (:timed-out? r)))
            (is (< ms 5000) (str "took " ms " ms"))
            (is (gone-within? pid 5000) (str "the detached process " pid " survived the timeout"))
            (finally (kill! pid))))))))

(deftest a-detached-holder-is-a-timeout-even-when-the-program-exits-before-its-output-is-read
  (with-temp-dir [dir]
    (let [[r _ pid] (with-delayed-threads (constantly 200) #(detached-holder dir))]
      (try
        (is (= {:exit 124 :timed-out? true} (select-keys r [:exit :timed-out?])))
        (is (gone-within? pid 5000) (str "the detached process " pid " survived the timeout"))
        (finally (kill! pid))))))

(deftest a-detached-holder-is-a-timeout-every-time
  (with-temp-dir [dir]
    (let [runs (with-delayed-threads #(rand-int 30)
                 #(vec (repeatedly 15 (fn [] (detached-holder dir "" {:timeout-ms 200})))))]
      (try
        (is (= [[124 true]] (distinct (map (fn [[r]] [(:exit r) (:timed-out? r)]) runs))))
        (is (every? (fn [[_ _ pid]] (gone-within? pid 5000)) runs))
        (finally (run! (fn [[_ _ pid]] (kill! pid)) runs))))))

(deftest a-detached-child-that-closed-the-output-does-not-delay-the-run
  (with-temp-dir [dir]
    (let [[r ms pid] (detached-holder dir ">/dev/null 2>&1" {:timeout-ms 10000})]
      (try
        (is (= {:exit 0 :timed-out? false} (select-keys r [:exit :timed-out?])))
        (is (< ms 5000) (str "took " ms " ms"))
        (finally (kill! pid))))))

(deftest output-written-before-a-late-read-is-kept
  (let [r (with-delayed-threads (constantly 200) #(process/run ["sh" "-c" "echo out; echo err >&2; exit 3"]))]
    (is (= {:exit 3 :out "out\n" :err "err\n" :timed-out? false}
           (select-keys r [:exit :out :err :timed-out?])))))

(deftest a-run-without-output-pipes-answers-127
  (with-redefs-fn {#'briko.process/make-fifos! (fn [_ _] "mkfifo could not run: missing")}
    #(let [r (process/run ["true"])]
       (is (= 127 (:exit r)))
       (is (false? (:launched? r)))
       (is (str/includes? (:err r) "could not create the output pipes")))))

(deftest the-output-pipes-are-removed
  ;; The directories each run used are recorded rather than globbed, so
  ;; other processes creating directories with the same prefix cannot
  ;; disturb the count.
  (let [used (atom [])
        run-with-pipes @#'briko.process/run-with-pipes]
    (with-redefs-fn {#'briko.process/run-with-pipes (fn [argv opts deadline dir]
                                                      (swap! used conj dir)
                                                      (run-with-pipes argv opts deadline dir))}
      (fn []
        (process/run ["echo" "x"])
        (process/run ["briko-no-such-program-0f3a"])
        (process/run ["sleep" "30"] {:timeout-ms 100})
        (with-temp-dir [dir]
          (process/run ["printf" "x"] {:out {:file (fs/path dir "out")}}))))
    (is (= 4 (count @used)))
    (is (not-any? fs/exists? @used))))

(deftest an-interrupted-wait-kills-the-tree-and-rethrows
  (with-temp-dir [dir]
    (let [pid-file (fs/path dir "pid")
          outcome (promise)
          t (Thread. ^Runnable
             (fn []
               (try
                 (process/run ["sh" "-c" (str "echo $$ > " pid-file "; exec sleep 300")]
                              {:timeout-ms 60000})
                 (deliver outcome :returned)
                 (catch InterruptedException _
                   (deliver outcome [:interrupted (.isInterrupted (Thread/currentThread))])))))]
      (.start t)
      (let [pid (read-pid pid-file)]
        (try
          (.interrupt t)
          (is (= [:interrupted true] (deref outcome 10000 :hung)))
          (is (gone-within? pid 5000))
          (finally (kill! pid)))))))

(deftest output-is-capped-per-stream
  (testing "stdout past the cap is drained and dropped"
    (let [r (process/run ["head" "-c" "100000" "/dev/zero"] {:max-out-bytes 1000})]
      (is (zero? (:exit r)))
      (is (= 1000 (count (:out r))))
      (is (true? (:out-truncated? r)))
      (is (false? (:err-truncated? r)))))
  (testing "stderr past the cap"
    (let [r (process/run ["sh" "-c" "head -c 5000 /dev/zero >&2"] {:max-err-bytes 10})]
      (is (= 10 (count (:err r))))
      (is (true? (:err-truncated? r)))))
  (testing "output of exactly the cap is complete"
    (let [r (process/run ["printf" "abcde"] {:max-out-bytes 5})]
      (is (= "abcde" (:out r)))
      (is (false? (:out-truncated? r)))))
  (testing "a cap of zero discards"
    (let [r (process/run ["echo" "x"] {:max-out-bytes 0})]
      (is (= "" (:out r)))
      (is (true? (:out-truncated? r)))))
  (testing "a program writing far more than a pipe holds still finishes"
    (let [r (process/run ["head" "-c" "5000000" "/dev/zero"])]
      (is (zero? (:exit r)))
      (is (= (get process/defaults :max-out-bytes) (count (:out r))))
      (is (true? (:out-truncated? r))))))

(deftest stdin-is-closed-unless-given
  (let [[r ms] (elapsed-ms #(process/run ["cat"] {:timeout-ms 10000}))]
    (is (= {:exit 0 :out ""} (select-keys r [:exit :out])))
    (is (< ms 5000)))
  (is (= "given" (:out (process/run ["cat"] {:in "given"}))))
  (is (= "é" (:out (process/run ["cat"] {:in (.getBytes "é" "UTF-8")}))))
  (testing "a program that does not read a large input"
    (is (zero? (:exit (process/run ["true"] {:in (str/join (repeat 1000000 "x"))}))))))

(deftest the-environment-can-be-extended-and-trimmed
  (is (= "set\n" (:out (process/run ["sh" "-c" "echo $BRIKO_TEST_VAR"] {:extra-env {"BRIKO_TEST_VAR" "set"}}))))
  (is (= "unset\n" (:out (process/run ["sh" "-c" "echo ${HOME-unset}"] {:unset-env ["HOME"]}))))
  (is (= "kept\n" (:out (process/run ["sh" "-c" "test -n \"$PATH\" && echo kept"])))))

(deftest runs-in-a-working-directory
  (with-temp-dir [dir]
    (let [real (str (fs/real-path dir))]
      (is (= (str real "\n") (:out (process/run ["pwd"] {:dir real}))))
      (is (= (str real "\n") (:out (process/run ["pwd"] {:dir (fs/file real)}))))
      (is (= (str real "\n") (:out (process/run ["pwd"] {:dir (fs/path real)})))))))

(deftest stderr-can-be-inherited
  (let [r (process/run ["sh" "-c" "echo to-the-parent >&2; echo out"] {:inherit-err? true})]
    (is (= {:exit 0 :out "out\n" :err ""} (select-keys r [:exit :out :err])))))

(deftest invalid-arguments-are-refused
  (doseq [argv [nil [] "echo hi" ["echo" 1] [:echo] ["" "x"] ["  "] ["echo" "a\u0000b"] {:cmd "x"}]]
    (is (= ::process/invalid-argv (refusal #(process/run argv))) (pr-str argv)))
  (doseq [opts [[] {:timeout 1} {:timeout-ms 0} {:timeout-ms -1} {:timeout-ms "1"} {:timeout-ms 1.5}
                {:timeout-ms (inc Integer/MAX_VALUE)}
                {:extra-env {"A" 1}} {:extra-env {:a "1"}} {:extra-env {"A=B" "1"}} {:extra-env ["A" "1"]}
                {:unset-env "HOME"} {:unset-env [:home]} {:unset-env {"A" "1"}}
                {:extra-env {"A" "1"} :unset-env ["A"]}
                {:dir 1} {:in 1} {:in [1]} {:max-out-bytes -1} {:max-err-bytes "1"} {:inherit-err? 1}
                {:timeout-ms nil} {:timeout-ms :never} {:timeout-ms "none"}
                {:in :terminal} {:in {:file 1}} {:in {:file ""}} {:in {:path "x"}} {:in {:file "x" :mode :r}}
                {:out "file"} {:out :file} {:out {:file ""}} {:out {:file "x" :append true}} {:out {:file "a\u0000b"}}
                {:out-keep :middle} {:err-keep "tail"} {:out-keep nil}
                {:on-start 1} {:on-start :start} {:on-start {}} {:on-start "f"}]]
    (is (= ::process/invalid-options (refusal #(process/run ["true"] opts))) (pr-str opts))))

(deftest a-refusal-does-not-echo-the-argv
  (try
    (process/run ["echo" "dummy-private-value" 1])
    (is false "expected a refusal")
    (catch clojure.lang.ExceptionInfo e
      (is (not (str/includes? (str (ex-message e) (pr-str (ex-data e))) "dummy-private-value"))))))

(deftest contradictory-options-are-refused
  (doseq [opts [{:out {:file "x"} :max-out-bytes 10}
                {:out {:file "x"} :out-keep :tail}
                {:out {:file "x"} :out-keep :head}
                {:inherit-err? true :err-keep :tail}
                {:inherit-err? true :max-err-bytes 10}]]
    (is (= ::process/invalid-options (refusal #(process/run ["true"] opts))) (pr-str opts)))
  (testing "the same options for a captured stream are accepted"
    (doseq [opts [{:out nil :max-out-bytes 10 :out-keep :tail}
                  {:inherit-err? false :max-err-bytes 10 :err-keep :tail}
                  {:inherit-err? true :max-out-bytes 10 :out-keep :tail}]]
      (is (zero? (:exit (process/run ["true"] opts))) (pr-str opts)))))

;; Deadlines

(deftest a-run-without-a-deadline-lasts-as-long-as-the-program
  (let [[r ms] (elapsed-ms #(process/run ["sh" "-c" "sleep 0.3; echo done"] {:timeout-ms :none}))]
    (is (= {:exit 0 :out "done\n" :timed-out? false} (select-keys r [:exit :out :timed-out?])))
    (is (>= ms 300))))

(deftest a-run-without-a-deadline-waits-for-a-detached-holder
  (with-temp-dir [dir]
    (let [pid-file (fs/path dir "pid")
          [r ms] (elapsed-ms #(process/run ["sh" "-c" (str "sleep 1 & echo $! > " pid-file)] {:timeout-ms :none}))]
      (is (= {:exit 0 :timed-out? false} (select-keys r [:exit :timed-out?])))
      (is (>= ms 900) (str "took " ms " ms"))
      (is (gone-within? (read-pid pid-file) 2000) "the holder ended before the run returned, up to its reaping"))))

(deftest an-interrupted-run-without-a-deadline-kills-the-tree
  (with-temp-dir [dir]
    (let [child (fs/path dir "child")
          grandchild (fs/path dir "grandchild")
          script (str "sleep 300 & echo $! > " child "; "
                      "sh -c 'sleep 300 & echo $! > " grandchild "; wait' & "
                      "wait")
          outcome (promise)
          t (Thread. ^Runnable
             (fn []
               (try
                 (process/run ["sh" "-c" script] {:timeout-ms :none})
                 (deliver outcome :returned)
                 (catch InterruptedException _
                   (deliver outcome :interrupted)))))]
      (.start t)
      (let [pids [(read-pid child) (read-pid grandchild)]]
        (try
          ;; Past the first samples, so the tree is known before the interrupt.
          (Thread/sleep 100)
          (.interrupt t)
          (is (= :interrupted (deref outcome 10000 :hung)))
          (doseq [pid pids]
            (is (gone-within? pid 5000) (str "process " pid " survived the interrupt")))
          (finally (run! kill! pids)))))))

;; Keeping the end of a stream

(defn- tail-of
  "What tail-sink keeps of chunks, a sequence of byte vectors, with limit."
  [limit chunks]
  (let [{:keys [consume finish]} (#'briko.process/tail-sink limit)]
    (doseq [c chunks]
      (let [bs (byte-array (map unchecked-byte c))]
        (consume bs (alength bs))))
    (update (finish) :bytes vec)))

(deftest the-tail-sink-keeps-the-last-bytes-at-every-boundary
  (do
    (is (= {:bytes [] :dropped? false} (tail-of 3 [])))
    (is (= {:bytes [1 2] :dropped? false} (tail-of 3 [[1 2]])))
    (is (= {:bytes [1 2 3] :dropped? false} (tail-of 3 [[1 2 3]])) "exactly the limit")
    (is (= {:bytes [1 2 3] :dropped? false} (tail-of 3 [[1] [2] [3]])) "exactly the limit in pieces")
    (is (= {:bytes [2 3 4] :dropped? true} (tail-of 3 [[1 2 3 4]])) "one past the limit")
    (is (= {:bytes [2 3 4] :dropped? true} (tail-of 3 [[1 2 3] [4]])) "one past the limit in pieces")
    (is (= {:bytes [7 8 9] :dropped? true} (tail-of 3 [[1 2] [3 4 5 6 7 8 9]])) "a chunk longer than the limit")
    (is (= {:bytes [] :dropped? true} (tail-of 0 [[1]])) "a limit of zero")
    (is (= {:bytes [] :dropped? false} (tail-of 0 [])))
    (testing "many small chunks across the buffer being cut back"
      (let [chunks (map vector (take 20000 (cycle (range 100))))]
        (is (= {:bytes (vec (take-last 10 (map first chunks))) :dropped? true} (tail-of 10 chunks)))))))

(deftest the-tail-sink-matches-the-end-of-the-stream
  (let [prop (prop/for-all [limit gen/nat
                            chunks (gen/vector (gen/vector (gen/choose 0 127)) 0 40)]
               (let [all (vec (apply concat chunks))]
                 (= {:bytes (vec (take-last limit all)) :dropped? (> (count all) limit)}
                    (tail-of limit chunks))))]
    (is (:pass? (tc/quick-check 300 prop)))))

(deftest a-cut-tail-starts-at-a-whole-character
  (is (= {:bytes [] :dropped? true} (tail-of 1 [[0x61 0xC3 0xA9]])) "half of é is dropped")
  (is (= {:bytes [0xC3 0xA9] :dropped? true} (update (tail-of 2 [[0x61 0xC3 0xA9]]) :bytes (partial mapv #(bit-and (long %) 0xFF)))))
  (is (= {:bytes [0xA9] :dropped? false} (update (tail-of 1 [[0xA9]]) :bytes (partial mapv #(bit-and (long %) 0xFF))))
      "an uncut stream is kept as it is"))

(deftest stderr-can-keep-its-end
  (let [expected (str/join (map #(str % "\n") (range 1 20001)))
        r (process/run ["sh" "-c" "seq 1 20000 >&2; exit 2"] {:err-keep :tail :max-err-bytes 20})]
    (is (= 2 (:exit r)))
    (is (= (subs expected (- (count expected) 20)) (:err r)))
    (is (true? (:err-truncated? r))))
  (testing "the head is still the default"
    (let [r (process/run ["sh" "-c" "seq 1 20000 >&2"] {:max-err-bytes 8})]
      (is (= "1\n2\n3\n4\n" (:err r))))))

(deftest stdout-can-keep-its-end
  (is (= {:out "abcde" :out-truncated? false}
         (select-keys (process/run ["printf" "abcde"] {:out-keep :tail :max-out-bytes 5}) [:out :out-truncated?])))
  (is (= {:out "bcde" :out-truncated? true}
         (select-keys (process/run ["printf" "abcde"] {:out-keep :tail :max-out-bytes 4}) [:out :out-truncated?])))
  (testing "a stream far longer than a pipe holds"
    (let [r (process/run ["sh" "-c" "head -c 3000000 /dev/zero; printf end"] {:out-keep :tail :max-out-bytes 3})]
      (is (= "end" (:out r)))
      (is (true? (:out-truncated? r))))))

;; Stdout into a file

(defn- mode-of
  [path]
  (java.nio.file.attribute.PosixFilePermissions/toString
   (java.nio.file.Files/getPosixFilePermissions (fs/path path) (into-array java.nio.file.LinkOption [java.nio.file.LinkOption/NOFOLLOW_LINKS]))))

(deftest stdout-can-go-to-a-file
  (with-temp-dir [dir]
    (let [target (fs/path dir "out")
          r (process/run ["sh" "-c" "head -c 5000000 /dev/zero; echo err >&2"] {:out {:file target}})]
      (is (= {:exit 0 :out nil :err "err\n" :out-bytes 5000000 :out-error nil :out-truncated? false}
             (select-keys r [:exit :out :err :out-bytes :out-error :out-truncated?])))
      (is (= 5000000 (fs/size target)))
      (is (= "rw-------" (mode-of target)) "created owner-only"))
    (testing "an existing file is truncated and keeps its mode"
      (let [target (fs/path dir "existing")]
        (spit (str target) (str/join (repeat 100 "old")))
        (fs/set-posix-file-permissions target "rw-r-----")
        (is (zero? (:exit (process/run ["printf" "new"] {:out {:file (str target)}}))))
        (is (= "new" (slurp (str target))))
        (is (= "rw-r-----" (mode-of target)))))
    (testing "a File is accepted"
      (let [target (fs/file dir "as-file")]
        (is (= 3 (:out-bytes (process/run ["printf" "abc"] {:out {:file target}}))))))))

(deftest a-stdout-file-that-cannot-be-opened-answers-127
  (with-temp-dir [dir]
    (let [victim (fs/path dir "victim")
          link (fs/path dir "link")
          fifo (str (fs/path dir "fifo"))]
      (spit (str victim) "keep")
      (fs/create-sym-link link victim)
      (is (zero? (:exit (process/run ["mkfifo" fifo]))))
      (doseq [[what target] [["a symbolic link" link]
                             ["a named pipe" fifo]
                             ["a directory" dir]
                             ["a missing parent directory" (fs/path dir "missing" "out")]]]
        (testing what
          (let [[r ms] (elapsed-ms #(process/run ["echo" "x"] {:out {:file target} :timeout-ms 5000}))]
            (is (= {:exit 127 :launched? false :out nil :out-bytes 0} (select-keys r [:exit :launched? :out :out-bytes])))
            (is (str/includes? (:err r) "stdout file"))
            (is (< ms 3000)))))
      (is (= "keep" (slurp (str victim))) "the link's target is untouched"))))

(deftest a-detached-child-holding-a-stdout-file-is-a-timeout
  (with-temp-dir [dir]
    (let [target (fs/path dir "out")
          [r ms pid] (detached-holder dir "" {:out {:file target}})]
      (try
        (is (= {:exit 124 :timed-out? true} (select-keys r [:exit :timed-out?])))
        (is (< ms 5000) (str "took " ms " ms"))
        (is (gone-within? pid 5000) (str "the detached process " pid " survived the timeout"))
        (finally (kill! pid))))))

(deftest a-file-sink-takes-nothing-once-finished
  (with-temp-dir [dir]
    (let [target (fs/path dir "out")
          {:keys [consume finish]} (#'briko.process/file-sink (#'briko.process/open-output-file target))
          bs (.getBytes "abc" "UTF-8")]
      (is (true? (consume bs 3)))
      (is (= {:written 3 :error nil} (finish)))
      (is (false? (boolean (consume bs 3))))
      (is (= "abc" (slurp (str target)))))))

(defn- failing-stream
  []
  (proxy [java.io.OutputStream] []
    (write
      ([_] (throw (java.io.IOException. "No space left on device")))
      ([_ _ _] (throw (java.io.IOException. "No space left on device"))))))

(deftest a-failed-write-to-the-stdout-file-is-reported
  (with-redefs-fn {#'briko.process/open-output-file (fn [_] (failing-stream))}
    ;; yes writes until a write fails, so it ends only because the pipe
    ;; lost its reader; a run that kept reading would end at the deadline.
    #(let [r (process/run ["yes"] {:out {:file "unused"} :timeout-ms 20000})]
       (is (str/includes? (str (:out-error r)) "No space left on device"))
       (is (zero? (:out-bytes r)))
       (is (false? (:timed-out? r)))
       (is (not (zero? (:exit r))) "the program's next write fails"))))

;; Stdin from a file or the terminal

(deftest stdin-can-come-from-a-file
  (with-temp-dir [dir]
    (let [source (fs/path dir "in")
          fifo (str (fs/path dir "fifo"))]
      (spit (str source) "from a file\n")
      (is (= "from a file\n" (:out (process/run ["cat"] {:in {:file source}}))))
      (is (= "from a file\n" (:out (process/run ["cat"] {:in {:file (str source)}}))))
      (is (zero? (:exit (process/run ["mkfifo" fifo]))))
      (doseq [[what target] [["a missing file" (fs/path dir "missing")]
                             ["a directory" dir]
                             ["a named pipe, which would block the launch" fifo]]]
        (testing what
          (let [[r ms] (elapsed-ms #(process/run ["cat"] {:in {:file target} :timeout-ms 5000}))]
            (is (= {:exit 127 :launched? false} (select-keys r [:exit :launched?])))
            (is (< ms 3000))))))))

(deftest stdin-can-come-from-the-terminal
  (testing "where there is no terminal"
    (with-redefs-fn {#'briko.process/tty-path "/nonexistent/briko-tty"}
      #(let [r (process/run ["cat"] {:in :tty})]
         (is (= {:exit 127 :launched? false} (select-keys r [:exit :launched?])))
         (is (str/includes? (:err r) "briko-tty")))))
  (testing "the terminal's path is what the program reads"
    (with-temp-dir [dir]
      (let [fake (fs/path dir "tty")]
        (spit (str fake) "typed")
        (with-redefs-fn {#'briko.process/tty-path (str fake)}
          #(is (= "typed" (:out (process/run ["cat"] {:in :tty}))))))))
  (testing "the real terminal, or 127 where this process has none"
    (let [has-tty? (try (.close (java.io.FileInputStream. "/dev/tty")) true (catch java.io.IOException _ false))
          r (process/run ["sh" "-c" "test -t 0"] {:in :tty})]
      (if has-tty?
        (is (zero? (:exit r)))
        (is (= 127 (:exit r)))))))

;; Stopping a run from outside

(defn- run-in-thread
  "Starts (process/run argv opts) on a thread of its own. Returns a promise
   of its result, or of [:threw e]."
  [argv opts]
  (let [outcome (promise)]
    (doto (Thread. ^Runnable
           (fn []
             (try
               (deliver outcome (process/run argv opts))
               (catch Exception e
                 (deliver outcome [:threw e])))))
      (.start))
    outcome))

(deftest a-shutdown-hook-can-stop-a-run-without-a-deadline
  (with-temp-dir [dir]
    (let [self (fs/path dir "self")
          child (fs/path dir "child")
          grandchild (fs/path dir "grandchild")
          script (str "echo $$ > " self "; echo started; "
                      "sleep 300 & echo $! > " child "; "
                      "sh -c 'sleep 300 & echo $! > " grandchild "; wait' & "
                      "wait")
          handle (promise)
          outcome (run-in-thread ["sh" "-c" script] {:timeout-ms :none :on-start (fn [h] (deliver handle h))})
          pids [(read-pid self) (read-pid child) (read-pid grandchild)]]
      (try
        (is (= (first pids) (:pid (deref handle 5000 nil))) "the handle names the program")
        (is (= :running (deref outcome 300 :running)) "negative control: without a stop the run goes on")
        ;; A shutdown hook is a Thread the JVM starts on exit; this one is
        ;; started by hand.
        (let [hook (Thread. ^Runnable (fn [] ((:stop @handle))))
              [r ms] (elapsed-ms (fn [] (.start hook) (.join hook 10000) (deref outcome 10000 :hung)))]
          (is (map? r) (pr-str r))
          (is (= {:exit 137 :stopped? true :timed-out? false :launched? true}
                 (select-keys r [:exit :stopped? :timed-out? :launched?])))
          (is (= "started\n" (:out r)) "output written before the stop is kept")
          (is (< ms 5000) (str "took " ms " ms")))
        (doseq [pid pids]
          (is (gone-within? pid 5000) (str "process " pid " survived the stop")))
        (finally (run! kill! pids))))))

(deftest a-stop-kills-a-detached-holder-of-the-output
  (with-temp-dir [dir]
    (let [pid-file (fs/path dir "pid")
          handle (promise)
          outcome (run-in-thread ["sh" "-c" (str "sleep 300 & echo $! > " pid-file)]
                                 {:timeout-ms :none :on-start (fn [h] (deliver handle h))})
          pid (read-pid pid-file)]
      (try
        (is (= :running (deref outcome 300 :running)) "negative control: the holder keeps the run open")
        (is (true? ((:stop (deref handle 5000 nil)))))
        (let [r (deref outcome 10000 :hung)]
          (is (= {:exit 0 :stopped? true :timed-out? false} (select-keys r [:exit :stopped? :timed-out?]))
              "the program itself had exited; the stop ended the holder"))
        (is (gone-within? pid 5000) (str "the holder " pid " survived the stop"))
        (finally (kill! pid))))))

(deftest stop-is-idempotent-and-inert-once-the-run-returned
  (testing "concurrent stops: exactly one takes effect"
    (let [handle (promise)
          outcome (run-in-thread ["sleep" "300"] {:timeout-ms :none :on-start (fn [h] (deliver handle h))})
          {:keys [stop pid]} (deref handle 5000 nil)
          start (promise)
          answers (mapv (fn [_] (future @start (stop))) (range 8))]
      (try
        (deliver start true)
        (is (= [false false false false false false false true] (sort (map deref answers))))
        (is (false? (stop)))
        (is (true? (:stopped? (deref outcome 10000 :hung))))
        (finally (kill! pid)))))
  (testing "after a run finished on its own"
    (let [handle (atom nil)
          r (process/run ["echo" "hi"] {:on-start (fn [h] (reset! handle h))})]
      (is (= {:exit 0 :out "hi\n" :stopped? false} (select-keys r [:exit :out :stopped?])))
      (is (false? ((:stop @handle))))
      (is (false? ((:stop @handle))))))
  (testing "after a timeout"
    (let [handle (atom nil)
          r (process/run ["sleep" "300"] {:timeout-ms 200 :on-start (fn [h] (reset! handle h))})]
      (is (= {:exit 124 :timed-out? true :stopped? false} (select-keys r [:exit :timed-out? :stopped?])))
      (is (false? ((:stop @handle)))))))

(deftest a-run-without-on-start-keeps-its-result
  (let [plain (process/run ["sh" "-c" "echo out; echo err >&2; exit 3"])
        watched (process/run ["sh" "-c" "echo out; echo err >&2; exit 3"] {:on-start (fn [_])})]
    (is (not (contains? plain :stopped?)))
    (is (= (assoc plain :stopped? false) watched))))

(deftest on-start-is-called-once-and-not-for-a-failed-launch
  (let [calls (atom [])]
    (process/run ["true"] {:on-start (fn [h] (swap! calls conj h))})
    (is (= 1 (count @calls)))
    (is (pos-int? (:pid (first @calls))))
    (is (fn? (:stop (first @calls)))))
  (let [calls (atom 0)
        r (process/run ["briko-no-such-program-0f3a"] {:on-start (fn [_] (swap! calls inc))})]
    (is (= 127 (:exit r)))
    (is (zero? @calls))))

(deftest a-callback-may-stop-its-own-run
  (let [r (process/run ["sleep" "300"] {:timeout-ms :none :on-start (fn [{:keys [stop]}] (stop))})]
    (is (= {:exit 137 :stopped? true} (select-keys r [:exit :stopped?])))))

(deftest a-throwing-callback-stops-the-program-and-refuses
  (doseq [[what thrown] [["an exception" (ex-info "callback broke" {:secret "dummy-value"})]
                         ["an error" (AssertionError. "callback asserted")]]]
    (testing what
      (let [pid (atom nil)
            handle (atom nil)
            [outcome ms] (elapsed-ms
                          #(try
                             (process/run ["sleep" "300"]
                                          {:timeout-ms :none
                                           :on-start (fn [h] (reset! handle h) (reset! pid (:pid h)) (throw thrown))})
                             :returned
                             (catch clojure.lang.ExceptionInfo e e)))]
        (try
          (is (= ::process/on-start-failed (:type (ex-data outcome))))
          (is (process/refusal? outcome))
          (is (identical? thrown (ex-cause outcome)))
          (is (not (str/includes? (str (ex-message outcome) (pr-str (ex-data outcome))) "dummy-value"))
              "the callback's data stays in the cause")
          (is (< ms 5000) (str "took " ms " ms"))
          (is (gone-within? @pid 5000) "the program survived its callback")
          (is (false? ((:stop @handle))))
          (finally (some-> @pid kill!)))))))

(deftest an-interrupted-callback-stops-the-program-and-rethrows
  (let [pid (atom nil)
        outcome (try
                  (process/run ["sleep" "300"]
                               {:timeout-ms :none
                                :on-start (fn [h] (reset! pid (:pid h)) (throw (InterruptedException. "hook")))})
                  :returned
                  (catch InterruptedException _
                    [:interrupted (Thread/interrupted)]))]
    (try
      (is (= [:interrupted true] outcome))
      (is (gone-within? @pid 5000))
      (finally (some-> @pid kill!)))))
