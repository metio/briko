;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-lines-test
  (:require [briko.http :as http]
            [briko.http-fixture :as f :refer [with-server]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.nio.charset StandardCharsets)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (http/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- caught
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- elapsed-ms
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(def ^:private stream-head "HTTP/1.1 200 OK\r\nContent-Type: application/x-ndjson\r\nConnection: close\r\n\r\n")

(defn- utf8
  ^bytes [^String s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- pieces
  "Answers 200 without a length and writes each piece (string or bytes)
   with a pause, then closes."
  [pause-ms & ps]
  (fn [_ out]
    (f/write! out stream-head)
    (doseq [p ps]
      (f/write! out p)
      (Thread/sleep (long pause-ms)))))

(defn- held-open
  "Answers 200, writes text, then keeps the connection open without
   sending anything until hold-ms passed."
  [^String text hold-ms]
  (fn [_ out]
    (f/write! out stream-head)
    (f/write! out (utf8 text))
    (Thread/sleep (long hold-ms))))

(defn- endless-lines
  "Answers 200 and writes a numbered line every pause-ms until the
   connection breaks."
  [pause-ms]
  (fn [_ out]
    (f/write! out stream-head)
    (try (loop [i 0]
           (f/write! out (str "line " i "\n"))
           (Thread/sleep (long pause-ms))
           (recur (inc i)))
         (catch Exception _ nil))))

;; Complete bodies

(deftest a-body-is-handed-over-line-by-line
  (let [seen (atom [])
        ;; a line end split across writes, \r\n, an empty line, a
        ;; multi-byte character split across writes and a last line
        ;; without an end
        a-umlaut (utf8 "ä")]
    (with-server [s (f/start! (pieces 20 "one\r" "\ntwo\n\n" (byte-array [(aget a-umlaut 0)])
                                      (byte-array [(aget a-umlaut 1)]) "x\r\nlast")
                              {})]
      (let [r (http/request :get (:base s) {:as :lines :on-line #(swap! seen conj %)})]
        (is (= 200 (:status r)))
        (is (= ["one" "two" "" "äx" "last"] @seen))
        (is (= 5 (:lines-read r)))
        (is (nil? (:body r)) "with :on-line the lines are not collected")
        (is (not (contains? r :truncated)))
        (is (= ["application/x-ndjson"] (get-in r [:headers "content-type"])))))))

(deftest without-on-line-the-lines-are-the-body
  (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "a\nb\r\nc\n")) {})]
    (let [r (http/request :get (:base s) {:as :lines})]
      (is (= ["a" "b" "c"] (:body r)))
      (is (= 3 (:lines-read r))))))

(deftest a-lone-carriage-return-is-part-of-the-line
  (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "a\rb\nc\r")) {})]
    (is (= ["a\rb" "c\r"] (:body (http/request :get (:base s) {:as :lines}))))))

(deftest malformed-utf-8-is-replaced
  (with-server [s (f/start! (pieces 0 (byte-array [97 (unchecked-byte 0xff) 10])) {})]
    (is (= ["a�"] (:body (http/request :get (:base s) {:as :lines}))))))

(deftest lines-arrive-while-the-stream-is-open
  (let [first-line (promise)
        seen (atom [])]
    ;; the server sends the second line only after :on-line saw the
    ;; first, so a reader that waited for the whole body would hang until
    ;; the deadline
    (with-server [s (f/start! (fn [_ out]
                                (f/write! out stream-head)
                                (f/write! out "first\n")
                                (when (deref first-line 5000 nil)
                                  (f/write! out "second\n")))
                              {})]
      (let [[r ms] (elapsed-ms #(http/request :get (:base s)
                                              {:as :lines :timeout-ms 8000
                                               :on-line (fn [line]
                                                          (swap! seen conj line)
                                                          (deliver first-line true))}))]
        (is (= ["first" "second"] @seen))
        (is (nil? (:truncated r)))
        (is (< ms 4000) (str ms " ms"))))))

;; The deadline

(deftest a-stream-open-at-the-deadline-answers-with-the-lines-read
  (with-server [s (f/start! (held-open "a\nb\nc\npartial" 5000) {})]
    (let [[r ms] (elapsed-ms #(http/request :get (:base s) {:as :lines :timeout-ms 800}))]
      (is (= 200 (:status r)))
      (is (= ["a" "b" "c"] (:body r)) "the incomplete line is dropped")
      (is (= 3 (:lines-read r)))
      (is (= :deadline (:truncated r)))
      (is (<= 750 ms 3000) (str ms " ms"))))
  (testing "control: the same stream in memory is refused at the deadline"
    (with-server [s (f/start! (held-open "a\nb\nc\npartial" 5000) {})]
      (is (= ::http/timeout (refusal #(http/request :get (:base s) {:as :text :timeout-ms 800})))))))

(deftest a-server-silent-before-the-headers-is-still-a-timeout
  (with-server [s (f/start! (fn [_ _] (Thread/sleep 3000)) {})]
    (is (= ::http/timeout (refusal #(http/request :get (:base s) {:as :lines :timeout-ms 400}))))))

(deftest on-line-is-not-called-after-the-request-returned
  (let [seen (atom 0)]
    (with-server [s (f/start! (endless-lines 10) {})]
      (let [r (http/request :get (:base s) {:as :lines :timeout-ms 600 :on-line (fn [_] (swap! seen inc))})
            at-return @seen]
        (Thread/sleep 400)
        (is (= :deadline (:truncated r)))
        (is (pos? (:lines-read r)))
        (is (= (:lines-read r) at-return @seen))))))

(deftest a-slow-on-line-call-finishes-before-the-request-returns
  (let [finished (atom [])]
    (with-server [s (f/start! (held-open "a\nb\n" 3000) {})]
      (let [r (http/request :get (:base s)
                            {:as :lines :timeout-ms 300
                             :on-line (fn [line]
                                        (when (= "b" line)
                                          ;; busy-wait: an interrupt would end Thread/sleep early
                                          (let [until (+ (System/nanoTime) 700000000)]
                                            (while (< (System/nanoTime) until))))
                                        (swap! finished conj line))})]
        (is (= ["a" "b"] @finished))
        (is (= 2 (:lines-read r)) "the call running at the deadline completed and counts")
        (is (= :deadline (:truncated r)))))))

;; A line already read when the deadline struck is not handed over once
;; the call running at that moment returns.
(deftest no-line-is-handed-over-after-the-deadline-struck
  (let [seen (atom [])]
    (with-server [s (f/start! (held-open "a\nb\nc\n" 3000) {})]
      (let [r (http/request :get (:base s)
                            {:as :lines :timeout-ms 300
                             :on-line (fn [line]
                                        (when (= "a" line)
                                          (let [until (+ (System/nanoTime) 600000000)]
                                            (while (< (System/nanoTime) until))))
                                        (swap! seen conj line))})]
        (Thread/sleep 200)
        (is (= ["a"] @seen))
        (is (= 1 (:lines-read r)))
        (is (= :deadline (:truncated r)))))))

;; Bounds

(deftest a-line-over-max-line-bytes-is-refused
  (let [ten (str/join (repeat 10 "x"))]
    (testing "exactly the limit, with either line end, is read"
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 (str ten "\n" ten "\r\n"))) {})]
        (is (= [ten ten] (:body (http/request :get (:base s) {:as :lines :max-line-bytes 10}))))))
    (testing "one byte more is refused"
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 (str "ok\n" ten "y\n"))) {})]
        (let [e (caught #(http/request :get (:base s) {:as :lines :max-line-bytes 10}))]
          (is (= ::http/line-too-long (:type (ex-data e))))
          (is (= 2 (:line (ex-data e))))
          (is (= 10 (:limit (ex-data e))))
          (is (not (str/includes? (ex-message e) "xxx"))))))
    (testing "the last line without an end is held to the limit too"
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 (str ten "y"))) {})]
        (is (= ::http/line-too-long (refusal #(http/request :get (:base s) {:as :lines :max-line-bytes 10}))))))
    (testing "a multi-byte character counts in bytes"
      (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "ääääää\n")) {})]
        (is (= ::http/line-too-long (refusal #(http/request :get (:base s) {:as :lines :max-line-bytes 10}))))))))

(deftest a-line-without-end-is-refused-without-waiting-for-it
  (let [endless (fn [_ out]
                  (f/write! out stream-head)
                  (let [chunk (byte-array 65536 (byte 97))]
                    (try (loop [] (f/write! out chunk) (recur))
                         (catch Exception _ nil))))]
    (with-server [s (f/start! endless {})]
      (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s)
                                                              {:as :lines :max-line-bytes 1000
                                                               :max-bytes (* 1024 1024 1024)
                                                               :timeout-ms 10000}))))]
        (is (= ::http/line-too-long r))
        (is (< ms 5000) (str ms " ms"))))))

(deftest truncate-cuts-a-long-line-at-a-character-boundary
  (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "aäää\nshort\nbbbbbbbbbbbbbbbbbbbbbbbb\r\n")) {})]
    (let [r (http/request :get (:base s) {:as :lines :max-line-bytes 4 :on-long-line :truncate})]
      ;; "a" plus 1.5 two-byte characters fits 4 bytes, so the cut falls
      ;; before the split character
      (is (= ["aä" "shor" "bbbb"] (:body r)))
      (is (= 3 (:lines-read r)))
      (is (= 3 (:lines-truncated r)))))
  (testing "control: without a long line nothing is truncated"
    (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "ab\ncd\n")) {})]
      (let [r (http/request :get (:base s) {:as :lines :max-line-bytes 4 :on-long-line :truncate})]
        (is (= ["ab" "cd"] (:body r)))
        (is (zero? (:lines-truncated r)))))))

(deftest max-lines-stops-an-open-stream
  (with-server [s (f/start! (held-open "1\n2\n3\n4\n" 5000) {})]
    (let [[r ms] (elapsed-ms #(http/request :get (:base s) {:as :lines :max-lines 2 :timeout-ms 8000}))]
      (is (= ["1" "2"] (:body r)))
      (is (= 2 (:lines-read r)))
      (is (= :max-lines (:truncated r)))
      (is (< ms 3000) (str ms " ms"))))
  (testing "control: fewer lines than the bound end with the body"
    (with-server [s (f/start! (fn [_ out] (f/respond! out 200 "1\n2\n")) {})]
      (let [r (http/request :get (:base s) {:as :lines :max-lines 5})]
        (is (= ["1" "2"] (:body r)))
        (is (nil? (:truncated r)))))))

(deftest max-bytes-bounds-the-whole-stream
  (with-server [s (f/start! (endless-lines 0) {})]
    (is (= ::http/too-large (refusal #(http/request :get (:base s) {:as :lines :max-bytes 10000 :timeout-ms 10000})))))
  (testing "a stated length over the limit is refused before reading"
    (with-server [s (f/start! (fn [_ out] (f/respond! out 200 (str/join (repeat 200 "a\n")))) {})]
      (is (= ::http/too-large (refusal #(http/request :get (:base s) {:as :lines :max-bytes 100}))))
      (is (= ::http/too-large (refusal #(http/request :get (:base s) {:as :lines :max-bytes 100 :on-line (fn [_])}))))))
  (testing "control: within the limit"
    (with-server [s (f/start! (fn [_ out] (f/respond! out 200 (str/join (repeat 20 "a\n")))) {})]
      (is (= 20 (:lines-read (http/request :get (:base s) {:as :lines :max-bytes 100})))))))

;; :on-line failures

(deftest a-throwing-on-line-stops-the-request-without-its-cause
  (let [seen (atom [])]
    (with-server [s (f/start! (held-open "secret-a\nsecret-b\nsecret-c\n" 5000) {})]
      (let [[e ms] (elapsed-ms #(caught (fn [] (http/request :get (str (:base s) "/watch?token=q-value")
                                                             {:as :lines :timeout-ms 8000
                                                              :on-line (fn [line]
                                                                         (swap! seen conj line)
                                                                         (when (= "secret-b" line)
                                                                           (throw (ex-info (str "bad " line) {:line line}))))}))))]
        (is (= ::http/on-line-failed (:type (ex-data e))))
        (is (= 2 (:line (ex-data e))))
        (is (nil? (ex-cause e)) "the exception, which quotes the line, is not kept")
        (is (not (str/includes? (pr-str (ex-data e)) "secret")))
        (is (not (str/includes? (ex-message e) "secret")))
        (is (not (str/includes? (pr-str (ex-data e)) "q-value")))
        (is (= ["secret-a" "secret-b"] @seen) "no line is handed over after the throw")
        (is (< ms 3000) (str ms " ms: the open stream did not hold the refusal"))))))

;; Other statuses

(deftest a-non-2xx-answer-comes-back-as-text
  (let [called (atom 0)]
    (with-server [s (f/start! (fn [_ out] (f/respond! out 503 "line one\nline two\n")) {})]
      (let [r (http/request :get (:base s) {:as :lines :on-line (fn [_] (swap! called inc))})]
        (is (= 503 (:status r)))
        (is (= "line one\nline two\n" (:body r)))
        (is (zero? @called))
        (is (not (contains? r :lines-read)))))))

;; Options

(deftest invalid-line-options-are-refused
  (doseq [[why method opts] [["on-line without :as :lines" :get {:on-line identity}]
                             ["max-lines without :as :lines" :get {:as :text :max-lines 5}]
                             ["max-line-bytes with a file" :get {:as {:file "/tmp/x"} :max-line-bytes 5}]
                             ["on-long-line without :as :lines" :get {:on-long-line :truncate}]
                             ["on-line not a function" :get {:as :lines :on-line 5}]
                             ["zero max-lines" :get {:as :lines :max-lines 0}]
                             ["zero max-line-bytes" :get {:as :lines :max-line-bytes 0}]
                             ["unknown long-line mode" :get {:as :lines :on-long-line :drop}]
                             ["HEAD has no lines" :head {:as :lines}]]]
    (testing why
      (is (= ::http/invalid-options (refusal #(http/request method "https://127.0.0.1:1/" opts))))))
  (testing "control: valid line options reach the network"
    (is (= ::http/connect-failed
           (refusal #(http/request :get "https://127.0.0.1:1/" {:as :lines :on-line identity :max-lines 3
                                                                :max-line-bytes 10 :on-long-line :truncate
                                                                :timeout-ms 2000}))))))
