;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-idle-test
  (:require [babashka.fs :as fs]
            [briko.http :as http]
            [briko.http-fixture :as f :refer [with-server]]
            [clojure.test :refer [deftest is testing]]))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-http-idle-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (http/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- elapsed-ms
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(defn- names
  [dir]
  (sort (map (comp str fs/file-name) (fs/list-dir dir))))

(defn- stall-after
  "Answers 200 with a stated length of 1000, sends text and then nothing
   for hold-ms."
  [^String text hold-ms]
  (fn [_ out]
    (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\nConnection: close\r\n\r\n")
    (f/write! out text)
    (Thread/sleep (long hold-ms))))

(defn- trickle
  "Answers 200 with n bytes, one byte (a line with :lines?) every
   pause-ms."
  [n pause-ms lines?]
  (fn [_ out]
    (f/write! out (str "HTTP/1.1 200 OK\r\n"
                       (when-not lines? (str "Content-Length: " n "\r\n"))
                       "Connection: close\r\n\r\n"))
    (try (dotimes [_ n] (f/write! out (if lines? "a\n" "a")) (Thread/sleep (long pause-ms)))
         (catch Exception _ nil))))

(deftest a-stalled-body-in-memory-is-refused-at-the-idle-timeout
  (doseq [as [:bytes :text]]
    (testing as
      (with-server [s (f/start! (stall-after "abc" 5000) {})]
        (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:as as :idle-timeout-ms 400
                                                                                :timeout-ms 10000}))))]
          (is (= ::http/idle-timeout r))
          (is (<= 350 ms 3000) (str ms " ms"))))))
  (testing "control: a body that keeps arriving outlasts the idle timeout"
    (with-server [s (f/start! (trickle 12 100 false) {})]
      (let [[r ms] (elapsed-ms #(http/request :get (:base s) {:as :text :idle-timeout-ms 600 :timeout-ms 10000}))]
        (is (= "aaaaaaaaaaaa" (:body r)))
        (is (< 600 ms) (str ms " ms: the transfer took longer than the idle timeout"))))))

(deftest waiting-for-the-headers-counts-as-idle
  (with-server [s (f/start! (fn [_ _] (Thread/sleep 5000)) {})]
    (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:idle-timeout-ms 400 :timeout-ms 10000}))))]
      (is (= ::http/idle-timeout r))
      (is (< ms 3000) (str ms " ms"))))
  (testing "control: without it the deadline cuts the wait off"
    (with-server [s (f/start! (fn [_ _] (Thread/sleep 5000)) {})]
      (is (= ::http/timeout (refusal #(http/request :get (:base s) {:timeout-ms 400})))))))

(deftest the-deadline-wins-when-it-comes-first
  (with-server [s (f/start! (stall-after "abc" 5000) {})]
    (is (= ::http/timeout (refusal #(http/request :get (:base s) {:idle-timeout-ms 3000 :timeout-ms 400}))))))

(deftest the-idle-refusal-names-the-limit-and-not-the-query
  (with-server [s (f/start! (stall-after "abc" 3000) {})]
    (let [e (try (http/request :get (str (:base s) "/x?token=q-value") {:idle-timeout-ms 300})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= ::http/idle-timeout (:type (ex-data e))))
      (is (= 300 (:limit (ex-data e))))
      (is (= (str (:base s) "/x") (:url (ex-data e))))
      (is (not (re-find #"q-value" (str (ex-message e) (pr-str (ex-data e)))))))))

(deftest a-stalled-download-is-refused-and-its-staged-file-removed
  (with-temp-dir [dir]
    (let [target (str dir "/out")]
      (spit target "kept")
      (with-server [s (f/start! (stall-after "abc" 5000) {})]
        (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:as {:file target}
                                                                                :idle-timeout-ms 400
                                                                                :timeout-ms 10000}))))]
          (is (= ::http/idle-timeout r))
          (is (< ms 4000) (str ms " ms"))
          (is (= "kept" (slurp target)))
          (is (= ["out"] (names dir)) "no staged file is left")))
      (testing "control: a download that keeps arriving is written"
        (with-server [s (f/start! (trickle 8 100 false) {})]
          (http/request :get (:base s) {:as {:file target} :idle-timeout-ms 600 :timeout-ms 10000})
          (is (= "aaaaaaaa" (slurp target))))))))

(deftest a-stalled-line-stream-is-refused-not-handed-over
  (with-server [s (f/start! (fn [_ out]
                              (f/write! out "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\na\nb\n")
                              (Thread/sleep 5000))
                            {})]
    (is (= ::http/idle-timeout (refusal #(http/request :get (:base s) {:as :lines :idle-timeout-ms 400
                                                                       :timeout-ms 10000})))))
  (testing "control: a stream that keeps sending lines reaches the deadline and answers"
    (with-server [s (f/start! (trickle 100 100 true) {})]
      (let [r (http/request :get (:base s) {:as :lines :idle-timeout-ms 600 :timeout-ms 1200})]
        (is (= :deadline (:truncated r)))
        (is (< 3 (:lines-read r)))))))

(deftest invalid-idle-timeouts-are-refused
  (doseq [v [0 -1 "5" 1.5]]
    (is (= ::http/invalid-options (refusal #(http/request :get "https://127.0.0.1:1/" {:idle-timeout-ms v}))))))
