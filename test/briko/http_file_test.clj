;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-file-test
  (:require [babashka.fs :as fs]
            [briko.fs :as bfs]
            [briko.http :as http]
            [briko.http-fixture :as f :refer [with-server]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-http-file-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (http/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- names
  "The names in dir, sorted."
  [dir]
  (sort (map (comp str fs/file-name) (fs/list-dir dir))))

(defn- staged
  "The names in dir briko.fs stages."
  [dir]
  (filter bfs/staged? (names dir)))

(defn- elapsed-ms
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(defn- unlength
  "Answers 200 with body and no Content-Length, closing at the end."
  [^String body]
  (fn [_ out]
    (f/write! out "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n")
    (f/write! out body)))

(deftest the-staged-file-check-sees-a-staged-file
  (with-temp-dir [dir]
    (spit (str dir "/.out.briko-0123456789abcdef0123456789abcdef.tmp") "x")
    (is (= 1 (count (staged dir))) "control: the check the tests below rely on finds a staged file")))

;; Streaming to a file

(deftest a-body-streams-to-the-file-and-the-path-comes-back
  (with-temp-dir [dir]
    (let [target (str dir "/out.bin")]
      (spit target "old")
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 {"Content-Type" "text/plain"} "fresh content")) {})]
        (let [r (http/request :get (:base s) {:as {:file target}})]
          (is (= 200 (:status r)))
          (is (= target (:body r)))
          (is (= ["text/plain"] (get-in r [:headers "content-type"])))
          (is (= "fresh content" (slurp target)))
          (is (= ["out.bin"] (names dir))))))))

(deftest a-body-larger-than-the-memory-limit-streams-to-the-file
  (with-temp-dir [dir]
    (let [target (str dir "/big.bin")
          chunk (byte-array 65536 (byte 98))
          n (* 320 65536)]
      (with-server [s (f/start! (fn [_ out]
                                  (f/write! out (str "HTTP/1.1 200 OK\r\nContent-Length: " n "\r\n\r\n"))
                                  (dotimes [_ 320] (f/write! out chunk)))
                                {})]
        (is (= ::http/too-large (refusal #(http/request :get (:base s))))
            "control: the default limit refuses this body in memory")
        (is (= target (:body (http/request :get (:base s) {:as {:file target :fsync? false}
                                                           :max-bytes (* 32 1024 1024)}))))
        (is (= n (fs/size target)))
        (is (empty? (staged dir)))))))

(deftest max-bytes-may-pass-the-array-bound-only-for-a-file
  (with-temp-dir [dir]
    (is (= ::http/invalid-options
           (refusal #(http/request :get "https://127.0.0.1:1/" {:max-bytes Integer/MAX_VALUE}))))
    (is (= ::http/connect-failed
           (refusal #(http/request :get "https://127.0.0.1:1/" {:max-bytes (* 8 1024 1024 1024)
                                                                :as {:file (str dir "/x")}
                                                                :timeout-ms 2000})))
        "the limit is accepted, the request then fails at the closed port")))

(deftest the-progress-function-sees-the-bytes-and-the-stated-length
  (with-temp-dir [dir]
    (let [seen (atom [])
          body (str/join (repeat 200000 "x"))]
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 body)) {})]
        (http/request :get (:base s) {:as {:file (str dir "/p") :on-progress (fn [n total] (swap! seen conj [n total]))}}))
      (is (seq @seen))
      (is (= [200000 200000] (last @seen)))
      (is (apply < (map first @seen)))
      (is (every? #(= 200000 (second %)) @seen)))
    (testing "without a stated length the total is nil"
      (let [seen (atom [])]
        (with-server [s (f/start! (unlength "abc") {})]
          (http/request :get (:base s) {:as {:file (str dir "/q") :on-progress (fn [n total] (swap! seen conj [n total]))}}))
        (is (= [[3 nil]] @seen))))))

(deftest only-a-2xx-answer-is-written
  (with-temp-dir [dir]
    (let [target (str dir "/out")]
      (spit target "kept")
      (with-server [s (f/start! (fn [_ out] (f/respond! out 404 "not found page")) {})]
        (let [r (http/request :get (:base s) {:as {:file target}})]
          (is (= 404 (:status r)))
          (is (nil? (:body r)))
          (is (= "kept" (slurp target)))
          (is (= ["out"] (names dir))))))))

(deftest a-body-over-the-limit-leaves-the-file-untouched
  (with-temp-dir [dir]
    (let [target (str dir "/out")
          body (str/join (repeat 1000 "a"))]
      (spit target "kept")
      (testing "exactly the limit is written"
        (with-server [s (f/start! (unlength body) {})]
          (http/request :get (:base s) {:as {:file target} :max-bytes 1000})
          (is (= body (slurp target))))
        (spit target "kept"))
      (testing "one byte more without a stated length is refused while streaming"
        (with-server [s (f/start! (unlength (str body "a")) {})]
          (is (= ::http/too-large (refusal #(http/request :get (:base s) {:as {:file target} :max-bytes 1000}))))))
      (testing "an endless body is refused at the limit"
        (with-server [s (f/start! (fn [_ out]
                                    (f/write! out "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n")
                                    (let [chunk (byte-array 65536 (byte 97))]
                                      (try (loop [] (f/write! out chunk) (recur))
                                           (catch Exception _ nil))))
                                  {})]
          (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:as {:file target} :max-bytes 100000}))))]
            (is (= ::http/too-large r))
            (is (< ms 5000)))))
      (testing "a stated length over the limit is refused before a file is staged"
        (with-server [s (f/start! (fn [_ out]
                                    (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 1000000000\r\n\r\n")
                                    (Thread/sleep 3000))
                                  {})]
          (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:as {:file target} :max-bytes 1000}))))]
            (is (= ::http/too-large r))
            (is (< ms 2000)))))
      (is (= "kept" (slurp target)))
      (is (= ["out"] (names dir))))))

(deftest a-trickled-body-is-cut-off-at-the-deadline-and-the-staged-file-removed
  (with-temp-dir [dir]
    (let [target (str dir "/out")
          trickle (fn [n] (fn [_ out]
                            (f/write! out (str "HTTP/1.1 200 OK\r\nContent-Length: " n "\r\n\r\n"))
                            (try (dotimes [_ n] (f/write! out "a") (Thread/sleep 100))
                                 (catch Exception _ nil))))]
      (spit target "kept")
      (with-server [s (f/start! (trickle 1000) {})]
        (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:as {:file target} :timeout-ms 800}))))]
          (is (= ::http/timeout r))
          (is (<= 750 ms 2800) (str ms " ms"))
          (is (= "kept" (slurp target)))
          (is (= ["out"] (names dir)) "no staged file is left when request returns")))
      (testing "control: the same trickle inside the deadline is written"
        (with-server [s (f/start! (trickle 5) {})]
          (http/request :get (:base s) {:as {:file target} :timeout-ms 5000})
          (is (= "aaaaa" (slurp target))))))))

(deftest the-staged-file-is-gone-before-a-timeout-is-reported-even-when-removing-it-is-slow
  (with-temp-dir [dir]
    (let [target (str dir "/out")
          copy @#'briko.http/copy-limited!
          slow-failure (fn [& args]
                         (try (apply copy args)
                              (catch Exception e
                                ;; the thread is interrupted, so Thread/sleep
                                ;; would return at once
                                (let [until (+ (System/nanoTime) 500000000)]
                                  (while (< (System/nanoTime) until)))
                                (throw e))))]
      (with-server [s (f/start! (fn [_ out]
                                  (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\n")
                                  (f/write! out "a")
                                  (Thread/sleep 3000))
                                {})]
        (with-redefs-fn {#'briko.http/copy-limited! slow-failure}
          (fn []
            (is (= ::http/timeout (refusal #(http/request :get (:base s) {:as {:file target} :timeout-ms 500}))))
            (is (= [] (names dir)) "the request waited for the staged file to be removed")))))))

(deftest a-connection-cut-mid-body-leaves-the-file-untouched
  (with-temp-dir [dir]
    (let [target (str dir "/out")
          cut (fn [reset?]
                (fn [_ out]
                  (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n")
                  (f/write! out (byte-array 1000 (byte 97)))
                  (Thread/sleep 100)
                  ;; the fixture closes the connection when the handler
                  ;; throws; with :linger-zero? the close sends a reset
                  (throw (ex-info "cut" {:reset? reset?}))))]
      (spit target "kept")
      (doseq [reset? [false true]]
        (testing (if reset? "reset" "closed")
          (with-server [s (f/start! (cut reset?) {:linger-zero? reset?})]
            (let [r (refusal #(http/request :get (:base s) {:as {:file target} :timeout-ms 5000}))]
              (is (= ::http/failed r))
              (is (= "kept" (slurp target)))
              (is (= ["out"] (names dir))))))))))

(deftest a-missing-directory-is-refused-before-sending
  (with-temp-dir [dir]
    (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "x")) {})]
      (let [e (try (http/request :get (:base s) {:as {:file (str dir "/missing/out")}}) nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= ::http/file-failed (:type (ex-data e))))
        (is (= ::bfs/no-directory (:reason (ex-data e))))
        (is (= (str dir "/missing/out") (:file (ex-data e)))))
      (is (empty? @(:requests s)) "the server was never asked"))))

(deftest a-directory-removed-while-streaming-is-a-file-failure
  (with-temp-dir [dir]
    (let [sub (str dir "/sub")
          _ (fs/create-dir sub)
          target (str sub "/out")]
      (with-server [s (f/start! (fn [_ out]
                                  (fs/delete-tree sub)
                                  (f/respond! out 200 "x"))
                                {})]
        (is (= ::http/file-failed (refusal #(http/request :get (:base s) {:as {:file target}}))))
        (is (= [] (names dir)))))))

(deftest invalid-file-sinks-are-refused
  (doseq [[why method as] [["no file" :get {}]
                           ["blank file" :get {:file " "}]
                           ["file of the wrong type" :get {:file 5}]
                           ["unknown key" :get {:file "/tmp/x" :mode "rw-------"}]
                           ["fsync not a boolean" :get {:file "/tmp/x" :fsync? "yes"}]
                           ["progress not a function" :get {:file "/tmp/x" :on-progress 5}]
                           ["HEAD has no body" :head {:file "/tmp/x"}]]]
    (testing why
      (is (= ::http/invalid-options (refusal #(http/request method "https://127.0.0.1:1/" {:as as})))))))

;; HEAD

(deftest a-head-answer-is-not-held-to-max-bytes-by-its-stated-length
  (let [handler (fn [_ out] (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 1000000000000\r\nConnection: close\r\n\r\n"))]
    (with-server [s (f/start! handler {})]
      (let [r (http/request :head (:base s) {:max-bytes 1000})]
        (is (= 200 (:status r)))
        (is (= "1000000000000" (http/header r "content-length")))
        (is (zero? (alength ^bytes (:body r))))))
    (testing "control: GET with the same answer is refused"
      (with-server [s (f/start! handler {})]
        (is (= ::http/too-large (refusal #(http/request :get (:base s) {:max-bytes 1000}))))))))

(deftest a-head-request-keeps-its-other-rules
  (is (= ::http/insecure-url (refusal #(http/request :head "http://example.invalid/"))))
  (with-server [s (f/start! (fn [_ _] (Thread/sleep 3000)) {})]
    (is (= ::http/timeout (refusal #(http/request :head (:base s) {:timeout-ms 300}))))))
