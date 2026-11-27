;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process-test
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
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

(deftest a-detached-child-holding-the-output-cannot-stall-the-caller
  (with-temp-dir [dir]
    (let [pid-file (fs/path dir "pid")
          [r ms] (elapsed-ms #(process/run ["sh" "-c" (str "sleep 300 & echo $! > " pid-file)]
                                           {:timeout-ms 500}))
          pid (read-pid pid-file)]
      (try
        (is (= 124 (:exit r)))
        (is (true? (:timed-out? r)))
        (is (< ms 5000) (str "took " ms " ms"))
        (finally (kill! pid))))))

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
                {:dir 1} {:in 1} {:in [1]} {:max-out-bytes -1} {:max-err-bytes "1"} {:inherit-err? 1}]]
    (is (= ::process/invalid-options (refusal #(process/run ["true"] opts))) (pr-str opts))))

(deftest a-refusal-does-not-echo-the-argv
  (try
    (process/run ["echo" "dummy-private-value" 1])
    (is false "expected a refusal")
    (catch clojure.lang.ExceptionInfo e
      (is (not (str/includes? (str (ex-message e) (pr-str (ex-data e))) "dummy-private-value"))))))
