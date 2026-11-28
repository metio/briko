;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process-start-test
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.net InetAddress ServerSocket)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute PosixFilePermissions)
           (java.util Optional)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (process/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- thrown
  "The ex-info (f) throws, or :no-refusal."
  [f]
  (try (f) :no-refusal (catch clojure.lang.ExceptionInfo e e)))

(defn- elapsed-ms
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(defn- pid-alive?
  [pid]
  (let [^Optional handle (java.lang.ProcessHandle/of (long pid))]
    (and (.isPresent handle) (.isAlive ^java.lang.ProcessHandle (.get handle)))))

(defn- gone-within?
  [pid ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (not (pid-alive? pid)) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 20) (recur))))))

(defn- within?
  "Whether (f) answers truthy within ms, polling."
  [f ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (f) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 20) (recur))))))

(defn- kill!
  [pid]
  (let [^Optional handle (java.lang.ProcessHandle/of (long pid))]
    (when (.isPresent handle)
      (.destroyForcibly ^java.lang.ProcessHandle (.get handle)))))

(defn- read-pid
  [file]
  (loop [n 0]
    (let [s (when (fs/exists? file) (str/trim (slurp (str file))))]
      (cond
        (not (str/blank? s)) (parse-long s)
        (< n 250) (do (Thread/sleep 20) (recur (inc n)))
        :else (throw (ex-info "no pid written" {:file (str file)}))))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (fs/create-temp-dir {:prefix "briko-start-test-"})]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- registered?
  [h]
  (boolean (some (fn [x] (identical? x h)) (vals @@#'briko.process/registry))))

(defn- free-port
  []
  (with-open [s (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
    (.getLocalPort s)))

(deftest a-started-program-runs-until-closed
  (let [h (process/start ["sleep" "300"])]
    (try
      (is (true? (:launched? h)))
      (is (pos-int? (:pid h)))
      (is (true? (process/alive? h)))
      (Thread/sleep 200)
      (is (true? (process/alive? h)) "negative control: it runs without a deadline")
      (is (= 137 (process/close h)))
      (is (false? (process/alive? h)))
      (is (gone-within? (:pid h) 5000))
      (finally (kill! (:pid h))))))

(deftest close-is-idempotent-and-concurrent-closes-agree
  (let [h (process/start ["sleep" "300"])
        go (promise)
        answers (mapv (fn [_] (future @go (process/close h))) (range 8))]
    (try
      (deliver go true)
      (is (= (repeat 8 137) (map deref answers)))
      (is (= 137 (process/close h)))
      (finally (kill! (:pid h))))))

(deftest close-kills-the-whole-tree
  (with-temp-dir [dir]
    (let [child (fs/path dir "child")
          grandchild (fs/path dir "grandchild")
          h (process/start ["sh" "-c" (str "sleep 300 & echo $! > " child "; "
                                           "sh -c 'sleep 300 & echo $! > " grandchild "; wait' & "
                                           "wait")])
          pids [(read-pid child) (read-pid grandchild)]]
      (try
        (is (every? pid-alive? pids) "negative control: the tree runs before the close")
        (is (= 137 (process/close h)))
        (doseq [pid pids]
          (is (gone-within? pid 5000) (str "process " pid " survived the close")))
        (finally (run! kill! (conj pids (:pid h))))))))

(deftest close-kills-a-detached-holder-of-the-output
  (with-temp-dir [dir]
    (let [pid-file (fs/path dir "pid")
          h (process/start ["sh" "-c" (str "sleep 300 & echo $! > " pid-file)])
          holder (read-pid pid-file)]
      (try
        (is (within? #(not (process/alive? h)) 5000) "the program itself exits")
        (Thread/sleep 100)
        (is (pid-alive? holder) "negative control: the holder outlives the program")
        (is (registered? h) "a held output keeps the program registered")
        (is (zero? (process/close h)) "the program's own exit")
        (is (gone-within? holder 5000) "the holder survived the close")
        (is (not (registered? h)))
        (finally (kill! holder))))))

(deftest a-program-that-exits-keeps-its-exit-and-output
  (let [h (process/start ["sh" "-c" "echo out; echo err >&2; exit 3"])]
    (is (within? #(not (registered? h)) 5000) "unregistered once it finished on its own")
    (is (= {:out "out\n" :err "err\n" :out-truncated? false :err-truncated? false}
           (process/output h)))
    (is (= 3 (process/close h)))
    (is (= 3 (process/close h)))))

(deftest output-keeps-a-bounded-end-and-never-blocks-the-program
  (testing "the end of a long stdout"
    (let [h (process/start ["seq" "1" "50000"] {:max-out-bytes 1000})]
      (is (within? #(not (registered? h)) 5000))
      (let [{:keys [out out-truncated?]} (process/output h)]
        (is (<= (count out) 1000))
        (is (str/ends-with? out "\n49999\n50000\n"))
        (is (true? out-truncated?)))
      (is (zero? (process/close h)))))
  (testing "a program writing without end keeps running and memory stays bounded"
    (let [h (process/start ["sh" "-c" "yes; exit 9"] {:max-out-bytes 100 :max-err-bytes 10})]
      (try
        (Thread/sleep 500)
        (is (true? (process/alive? h)) "a full pipe would have stopped or blocked it")
        (let [{:keys [out out-truncated?]} (process/output h)]
          (is (<= (count out) 100))
          (is (true? out-truncated?))
          (is (re-matches #"(y\n)*y?\n?" out)))
        (is (= 137 (process/close h)))
        (finally (kill! (:pid h))))))
  (testing "the end of stderr"
    (let [h (process/start ["sh" "-c" "seq 1 3000 >&2"] {:max-err-bytes 20})]
      (is (within? #(not (registered? h)) 5000))
      (let [{:keys [err err-truncated?]} (process/output h)]
        (is (= 20 (count err)))
        (is (str/ends-with? err "2999\n3000\n"))
        (is (true? err-truncated?)))
      (process/close h))))

(deftest stdin-is-fed-and-closed
  (let [h (process/start ["cat"] {:in "fed\n"})]
    (is (within? #(not (registered? h)) 5000) "cat ends at the end of its input")
    (is (= "fed\n" (:out (process/output h))))
    (is (zero? (process/close h)))))

(deftest stdout-can-go-to-a-file
  (with-temp-dir [dir]
    (let [target (fs/path dir "out")
          h (process/start ["sh" "-c" "echo one; echo two"] {:out {:file target}})]
      (is (within? #(not (registered? h)) 5000))
      (is (= {:out nil :out-bytes 8 :out-error nil} (select-keys (process/output h) [:out :out-bytes :out-error])))
      (is (= "one\ntwo\n" (slurp (str target))))
      (is (= "rw-------" (PosixFilePermissions/toString (Files/getPosixFilePermissions target (make-array LinkOption 0)))))
      (is (zero? (process/close h)))))
  (testing "a symbolic link is refused and its target untouched"
    (with-temp-dir [dir]
      (let [target (fs/path dir "target")
            link (fs/path dir "link")]
        (spit (str target) "keep")
        (fs/create-sym-link link target)
        (let [h (process/start ["echo" "x"] {:out {:file link}})]
          (is (false? (:launched? h)))
          (is (nil? (:pid h)))
          (is (= 127 (process/close h)))
          (is (str/includes? (:err (process/output h)) "not a regular file"))
          (is (= "keep" (slurp (str target)))))))))

(deftest a-program-that-cannot-start-gives-a-closed-handle
  (testing "a missing program"
    (let [h (process/start ["briko-no-such-program-0f3a"])]
      (is (false? (:launched? h)))
      (is (false? (process/alive? h)))
      (is (= 127 (process/close h)))
      (is (str/includes? (:err (process/output h)) "briko-no-such-program-0f3a"))
      (is (= ::process/exited-before-ready
             (refusal #(process/wait-ready h {:pattern #"x" :timeout-ms 1000}))))))
  (testing "no mkfifo"
    (with-redefs-fn {#'briko.process/make-fifos! (fn [_ _] "mkfifo could not run: missing")}
      #(let [h (process/start ["sleep" "300"])]
         (is (false? (:launched? h)))
         (is (= 127 (process/close h)))
         (is (str/includes? (:err (process/output h)) "could not create the output pipes"))))))

(deftest the-pipe-directory-is-gone-while-the-program-runs
  (let [used (atom nil)
        start-with-pipes @#'briko.process/start-with-pipes]
    (with-redefs-fn {#'briko.process/start-with-pipes (fn [argv opts dir]
                                                        (reset! used dir)
                                                        (start-with-pipes argv opts dir))}
      (fn []
        (let [h (process/start ["sh" "-c" "sleep 0.3; echo later; sleep 300"])]
          (try
            (is (not (fs/exists? @used)) "nothing left on disk once start returned")
            (is (identical? h (process/wait-ready h {:pattern #"^later$" :timeout-ms 5000}))
                "output still arrives through the removed pipes")
            (is (= 137 (process/close h)))
            (finally (kill! (:pid h)))))))))

;; wait-ready

(deftest wait-ready-finds-a-line-of-output
  (testing "a line on stdout written later"
    (let [h (process/start ["sh" "-c" "sleep 0.3; echo 'Forwarding from 127.0.0.1:8080 -> 80'; sleep 300"])]
      (try
        (is (identical? h (process/wait-ready h {:pattern #"^Forwarding from" :timeout-ms 5000})))
        (is (true? (process/alive? h)))
        (finally (process/close h)))))
  (testing "a line on stderr written before the wait"
    (let [h (process/start ["sh" "-c" "echo ready >&2; sleep 300"])]
      (try
        (Thread/sleep 300)
        (is (identical? h (process/wait-ready h {:pattern #"^ready$" :stream :err :timeout-ms 5000})))
        (finally (process/close h)))))
  (testing "a CRLF line ends before the CR"
    (let [h (process/start ["sh" "-c" "printf 'up\\r\\n'; sleep 300"])]
      (try
        (is (identical? h (process/wait-ready h {:pattern #"^up$" :timeout-ms 5000})))
        (finally (process/close h))))))

(deftest wait-ready-sees-only-the-latest-lines-before-it
  (let [script "echo ready-a; seq 1 40; echo ready-b; seq 1 40; sleep 300"]
    (testing "a ready line within the latest 64"
      (let [h (process/start ["sh" "-c" script])]
        (try
          (Thread/sleep 300)
          (is (identical? h (process/wait-ready h {:pattern #"^ready-b$" :timeout-ms 2000})))
          (finally (process/close h)))))
    (testing "a ready line pushed out before the wait"
      (let [h (process/start ["sh" "-c" script])]
        (try
          (Thread/sleep 300)
          (is (= ::process/ready-timeout (refusal #(process/wait-ready h {:pattern #"^ready-a$" :timeout-ms 300}))))
          (finally (process/close h)))))))

(deftest wait-ready-refuses-at-the-deadline-and-closes
  (with-temp-dir [dir]
    (let [child (fs/path dir "child")
          h (process/start ["sh" "-c" (str "echo nothing; sleep 300 & echo $! > " child "; wait")])
          pid (read-pid child)
          [e ms] (elapsed-ms #(thrown (fn [] (process/wait-ready h {:pattern #"^ready$" :timeout-ms 400}))))]
      (try
        (is (= ::process/ready-timeout (:type (ex-data e))))
        (is (= {:timeout-ms 400 :type ::process/ready-timeout} (ex-data e)))
        (is (<= 400 ms 5000) (str "took " ms " ms"))
        (is (false? (process/alive? h)))
        (is (gone-within? pid 5000) "the tree is closed with the program")
        (is (= 137 (process/close h)))
        (finally (kill! pid))))))

(deftest wait-ready-can-leave-the-program-running
  (let [h (process/start ["sleep" "300"])]
    (try
      (is (= ::process/ready-timeout
             (refusal #(process/wait-ready h {:pattern #"x" :timeout-ms 200 :close-on-refusal? false}))))
      (is (true? (process/alive? h)))
      (finally (process/close h)))))

(deftest wait-ready-refuses-a-program-that-exits-first
  (let [h (process/start ["sh" "-c" "echo dummy-value >&2; echo dummy-value; exit 4"])
        e (thrown #(process/wait-ready h {:pattern #"^ready$" :timeout-ms 5000}))]
    (is (= {:type ::process/exited-before-ready :exit 4} (ex-data e)))
    (is (not (str/includes? (str (ex-message e) (pr-str (ex-data e))) "dummy-value"))
        "the output stays out of the refusal")
    (is (str/includes? (:err (process/output h)) "dummy-value") "and in output")))

(deftest wait-ready-waits-for-a-loopback-port
  (testing "a port that starts accepting while it waits"
    (let [port (free-port)
          h (process/start ["sleep" "300"])
          server (future (Thread/sleep 300) (ServerSocket. port 1 (InetAddress/getByName "127.0.0.1")))]
      (try
        (let [[r ms] (elapsed-ms #(process/wait-ready h {:port port :timeout-ms 5000}))]
          (is (identical? h r))
          (is (>= ms 250) "negative control: not ready before the listener"))
        (finally
          (process/close h)
          (.close ^ServerSocket @server)))))
  (testing "a port nobody listens on"
    (let [h (process/start ["sleep" "300"])]
      (is (= ::process/ready-timeout (refusal #(process/wait-ready h {:port (free-port) :timeout-ms 300}))))
      (is (gone-within? (:pid h) 5000))))
  (testing "a port another program holds, while ours exits within the settle time"
    (with-open [foreign (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
      (let [h (process/start ["sh" "-c" "sleep 0.2; exit 255"])]
        (is (= {:type ::process/exited-before-ready :exit 255}
               (ex-data (thrown #(process/wait-ready h {:port (.getLocalPort foreign) :settle-ms 1000
                                                        :timeout-ms 5000})))))))))

(deftest an-interrupted-wait-closes-and-rethrows
  (let [h (process/start ["sleep" "300"])
        outcome (promise)
        t (Thread. ^Runnable
           (fn []
             (try
               (process/wait-ready h {:pattern #"never" :timeout-ms 60000})
               (deliver outcome :returned)
               (catch InterruptedException _
                 (deliver outcome [:interrupted (.isInterrupted (Thread/currentThread))])))))]
    (try
      (.start t)
      (Thread/sleep 200)
      (.interrupt t)
      (is (= [:interrupted true] (deref outcome 10000 :hung)))
      (is (gone-within? (:pid h) 5000))
      (finally (kill! (:pid h))))))

;; The registry and the shutdown hook

(deftest the-shutdown-hook-stops-every-started-program
  (with-temp-dir [dir]
    (let [child (fs/path dir "child")
          a (process/start ["sh" "-c" (str "sleep 300 & echo $! > " child "; wait")])
          b (process/start ["sleep" "300"])
          kept (process/start ["sleep" "300"] {:stop-on-exit? false})
          pids [(:pid a) (read-pid child) (:pid b)]]
      (try
        (is (registered? a))
        (is (registered? b))
        (is (not (registered? kept)) "a program the caller opted out stays unregistered")
        (is (every? pid-alive? pids) "negative control: they run before the hook")
        ;; The JVM starts the hook Thread on exit; this one runs the same
        ;; function, started by hand.
        (let [hook (Thread. ^Runnable (fn [] (#'briko.process/stop-all!)))
              [_ ms] (elapsed-ms (fn [] (.start hook) (.join hook 10000)))]
          (is (false? (.isAlive hook)) "the hook ends instead of keeping the JVM alive")
          (is (< ms 5000) (str "took " ms " ms")))
        (doseq [pid pids]
          (is (gone-within? pid 5000) (str "process " pid " survived the hook")))
        (is (not (registered? a)))
        (is (not (registered? b)))
        (is (true? (process/alive? kept)))
        (is (= 137 (process/close a)) "close after the hook answers the hook's exit")
        (finally
          (process/close kept)
          (run! kill! pids))))))

(deftest the-shutdown-hook-is-registered-once-and-not-started
  (let [h (process/start ["true"])
        hook @@#'briko.process/shutdown-hook]
    (process/close h)
    (process/close (process/start ["true"]))
    (is (identical? hook @@#'briko.process/shutdown-hook))
    (is (= "NEW" (str (.getState ^Thread hook))))
    (is (false? (.removeShutdownHook (Runtime/getRuntime) (Thread. ^Runnable (fn []))))
        "negative control: a thread never added is not a hook")
    (is (true? (.removeShutdownHook (Runtime/getRuntime) hook)) "the hook is registered")
    (.addShutdownHook (Runtime/getRuntime) hook)))

(deftest the-registry-does-not-grow
  (let [before (count @@#'briko.process/registry)]
    (dotimes [_ 5]
      (process/close (process/start ["sleep" "300"])))
    (let [hs (mapv (fn [_] (process/start ["true"])) (range 5))]
      (is (within? #(not-any? registered? hs) 5000))
      (run! process/close hs))
    (is (= before (count @@#'briko.process/registry)))))

;; Refusals

(deftest invalid-arguments-are-refused
  (is (= ::process/invalid-argv (refusal #(process/start []))))
  (is (= ::process/invalid-options (refusal #(process/start ["true"] {:timeout-ms 100}))) "a started program has no deadline")
  (is (= ::process/invalid-options (refusal #(process/start ["true"] {:stop-on-exit? "yes"}))))
  (is (= ::process/invalid-options (refusal #(process/start ["true"] {:out {:file "x"} :max-out-bytes 10}))))
  (is (= ::process/invalid-options (refusal #(process/start ["true"] {:inherit-err? true :max-err-bytes 10}))))
  (is (= ::process/invalid-handle (refusal #(process/close {}))))
  (is (= ::process/invalid-handle (refusal #(process/alive? nil))))
  (let [h (process/start ["sleep" "300"])
        e (process/start ["sleep" "300"] {:inherit-err? true})]
    (try
      (doseq [opts [nil
                    {:pattern #"a" :port 8080}
                    {:pattern "ready"}
                    {:port 0}
                    {:port 65536}
                    {:port 8080 :host "10.0.0.1"}
                    {:port 8080 :host "localhost"}
                    {:port 8080 :stream :err}
                    {:pattern #"a" :host "127.0.0.1"}
                    {:pattern #"a" :timeout-ms :none}
                    {:pattern #"a" :timeout-ms 0}
                    {:pattern #"a" :settle-ms -1}
                    {:pattern #"a" :stream :both}
                    {:pattern #"a" :closing true}]]
        (is (= ::process/invalid-options (refusal #(process/wait-ready h opts))) (pr-str opts)))
      (is (= ::process/invalid-options (refusal #(process/wait-ready e {:pattern #"a" :stream :err}))))
      (is (true? (process/alive? h)) "a refused option touches nothing")
      (finally
        (process/close h)
        (process/close e)))))
