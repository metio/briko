;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-test
  (:require [briko.http :as http]
            [briko.http-fixture :as f :refer [with-server]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.net InetAddress ServerSocket)))

;; A value that stands for a credential: it travels in an Authorization
;; header and a query string and must never reach a refusal.
(def ^:private marker "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08")

(defn- trust []
  (http/trust-context (f/certificate-pem)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (http/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- caught
  "The exception (f) throws, or nil."
  [f]
  (try (f) nil (catch Exception e e)))

(defn- elapsed-ms
  [f]
  (let [start (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) start) 1000000)]))

(defn- ok-handler
  [body]
  (fn [_ out] (f/respond! out 200 {"Content-Type" "text/plain"} body)))

(defn- redirect-handler
  "Redirects with status to location, or answers 200 \"final\" on /final."
  [status location]
  (fn [req out]
    (if (str/starts-with? (:path req) "/final")
      (f/respond! out 200 "final")
      (f/respond! out status {"Location" location} ""))))

(defn- free-port
  "A loopback port nothing listens on."
  []
  (let [s (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))
        port (.getLocalPort s)]
    (.close s)
    port))

;; Answers

(deftest a-loopback-http-request-answers-status-headers-and-body
  (with-server [s (f/start! (ok-handler "hello") {})]
    (let [r (http/request :get (str (:base s) "/x"))]
      (is (= 200 (:status r)))
      (is (bytes? (:body r)))
      (is (= "hello" (String. ^bytes (:body r) "UTF-8")))
      (is (= ["text/plain"] (get-in r [:headers "content-type"])))
      (is (= "text/plain" (http/header r "Content-Type")))
      (is (= (str (:base s) "/x") (:url r)))
      (is (http/ok? r)))))

(deftest as-text-decodes-utf-8
  (with-server [s (f/start! (ok-handler "grüße") {})]
    (is (= "grüße" (:body (http/request :get (:base s) {:as :text}))))))

(deftest an-https-request-with-the-supplied-trust-answers
  (with-server [s (f/start! (ok-handler "secure") {:tls? true})]
    (is (= "secure" (:body (http/request :get (:base s) {:ssl-context (trust) :as :text}))))
    (testing "localhost is in the certificate too"
      (is (= 200 (:status (http/request :get (str "https://localhost:" (:port s))
                                        {:ssl-context (trust)})))))))

(deftest a-self-signed-server-is-refused-under-the-system-trust
  (with-server [s (f/start! (ok-handler "secure") {:tls? true})]
    (is (= ::http/tls-failed (refusal #(http/request :get (:base s)))))))

(deftest a-certificate-for-another-host-is-refused
  (let [s (try (f/start! (ok-handler "x") {:tls? true :bind "127.0.0.2"})
               (catch Exception _ nil))]
    (if-not s
      (println "SKIPPED a-certificate-for-another-host-is-refused: 127.0.0.2 cannot be bound")
      (try
        (is (= ::http/tls-failed (refusal #(http/request :get (:base s) {:ssl-context (trust)}))))
        (finally ((:stop s)))))))

(deftest non-2xx-statuses-are-answers
  (with-server [s (f/start! (fn [_ out] (f/respond! out 503 "down")) {})]
    (let [r (http/request :get (:base s) {:as :text})]
      (is (= 503 (:status r)))
      (is (= "down" (:body r)))
      (is (not (http/ok? r))))))

(deftest method-body-and-query-reach-the-server
  (with-server [s (f/start! (ok-handler "") {})]
    (http/request :post (str (:base s) "/p?x=1")
                  {:body "{\"a\":1}" :headers {"Content-Type" "application/json" :x-extra "v"}
                   :query {:b "two words" "c" ["1" "2"]}})
    (let [req (first @(:requests s))]
      (is (= "POST" (:method req)))
      (is (= "/p?x=1&b=two+words&c=1&c=2" (:path req)))
      (is (= "{\"a\":1}" (:body req)))
      (is (= "application/json" (get-in req [:headers "content-type"])))
      (is (= "v" (get-in req [:headers "x-extra"]))))))

(deftest head-answers-without-a-body
  (with-server [s (f/start! (ok-handler "ignored") {})]
    (let [r (http/request :head (:base s))]
      (is (= 200 (:status r)))
      (is (zero? (alength ^bytes (:body r))))
      (is (= "HEAD" (:method (first @(:requests s))))))))

;; Options

(deftest invalid-options-are-refused
  (doseq [[why method opts] [["unknown option" :get {:timeout-millis 5}]
                             ["zero timeout" :get {:timeout-ms 0}]
                             ["negative connect timeout" :get {:connect-timeout-ms -1}]
                             ["timeout as a string" :get {:timeout-ms "5"}]
                             ["too many redirects allowed" :get {:max-redirects 21}]
                             ["unknown body type" :get {:as :stream}]
                             ["body on get" :get {:body "x"}]
                             ["body on head" :head {:body "x"}]
                             ["body of the wrong type" :post {:body 5}]
                             ["insecure flag not a boolean" :get {:allow-insecure-http? "yes"}]
                             ["ssl-context of the wrong type" :get {:ssl-context "tls"}]
                             ["unknown method" :trace {}]
                             ["options not a map" :get [:timeout-ms 5]]]]
    (testing why
      (is (= ::http/invalid-options (refusal #(http/request method "https://127.0.0.1:1/" opts)))))))

(deftest an-invalid-trust-is-refused
  (is (= ::http/invalid-trust (refusal #(http/trust-context "not a certificate"))))
  (is (= ::http/invalid-trust (refusal #(http/trust-context ""))))
  (is (= ::http/invalid-trust (refusal #(http/trust-context 5)))))

;; URLs and transport

(deftest loopback-hosts-are-recognised-without-a-resolver
  (doseq [host ["localhost" "LOCALHOST" "127.0.0.1" "127.1.2.3" "[::1]" "[0:0:0:0:0:0:0:1]" "[0::0:1]"
                "[0000:0000:0000:0000:0000:0000:0000:0001]"]]
    (is (true? (#'briko.http/loopback? host)) host))
  (doseq [host ["127.0.0.1.example.org" "localhost.example.org" "128.0.0.1" "[::2]" "example.org"
                "1270.0.0.1" "127.0.0.256" "0.0.0.0" "[::ffff:10.0.0.1]" "[::ffff:127.0.0.1]"
                "[::1%lo]" "[1::1]" "[::1::]" "[0:0:0:0:0:0:0:0:1]" "[::00001]" "[::]" "::1" "[::1"]]
    (is (false? (#'briko.http/loopback? host)) host)))

(deftest plain-http-to-a-host-that-is-not-loopback-is-refused-before-connecting
  (is (= ::http/insecure-url (refusal #(http/request :get "http://example.invalid/x"))))
  (is (= ::http/insecure-url (refusal #(http/request :get "http://10.0.0.1/x"))))
  (testing "the named option lifts the refusal; the request then fails at the resolver"
    (is (not= ::http/insecure-url
              (refusal #(http/request :get "http://example.invalid/x"
                                      {:allow-insecure-http? true :timeout-ms 2000}))))))

(deftest plain-http-to-loopback-is-allowed
  (with-server [s (f/start! (ok-handler "local") {})]
    (is (= 200 (:status (http/request :get (:base s)))))
    (is (= 200 (:status (http/request :get (str "http://localhost:" (:port s))))))))

(deftest invalid-urls-are-refused
  (doseq [url ["ftp://127.0.0.1/x" "file:///etc/passwd" "/relative" "https://" "not a url" "https:///path"]]
    (is (= ::http/invalid-url (refusal #(http/request :get url))) url)))

(deftest a-url-with-user-info-is-refused-without-naming-it
  (let [e (caught #(http/request :get (str "https://user:" marker "@127.0.0.1/x")))]
    (is (= ::http/invalid-url (:type (ex-data e))))
    (is (not (str/includes? (str (ex-message e) (pr-str (ex-data e))) marker)))))

(deftest a-closed-port-is-a-connection-failure
  (is (= ::http/connect-failed (refusal #(http/request :get (str "http://127.0.0.1:" (free-port) "/"))))))

;; Headers

(deftest headers-that-could-inject-are-refused-before-sending
  (with-server [s (f/start! (ok-handler "") {})]
    (doseq [[why headers] [["CR LF in a value" {"X-A" "v\r\nInjected: 1"}]
                           ["LF in a value" {"X-A" "v\nInjected: 1"}]
                           ["CR in a value" {"X-A" "v\rx"}]
                           ["NUL in a value" {"X-A" (str "v" (char 0))}]
                           ["DEL in a value" {"X-A" (str "v" (char 127))}]
                           ["a value that is not a string" {"X-A" 5}]
                           ["a space in a name" {"X A" "v"}]
                           ["a colon in a name" {"X-A:" "v"}]
                           ["CR LF in a name" {"X-A\r\nInjected" "v"}]
                           ["an empty name" {"" "v"}]
                           ["a name that is not a string" {5 "v"}]
                           ["a header the client sets" {"Host" "evil.example"}]
                           ["Content-Length" {"content-length" "0"}]
                           ["a name given twice" {"X-A" "1" "x-a" "2"}]]]
      (testing why
        (is (= ::http/invalid-header (refusal #(http/request :get (:base s) {:headers headers}))))))
    (is (empty? @(:requests s)) "no refused request reached the server")
    (testing "control: a valid header with a tab arrives"
      (http/request :get (:base s) {:headers {"X-A" "one\ttwo"}})
      (is (= "one\ttwo" (get-in (first @(:requests s)) [:headers "x-a"]))))))

;; Body limits

(deftest a-declared-body-over-the-limit-is-refused-before-reading
  (with-server [s (f/start! (fn [_ out]
                              (f/write! out "HTTP/1.1 200 OK\r\nContent-Length: 1000000000\r\n\r\n")
                              (Thread/sleep 3000))
                            {})]
    (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:max-bytes 1000}))))]
      (is (= ::http/too-large r))
      (is (< ms 2000) "refused on the header, not after waiting for the body"))))

(deftest an-endless-body-without-a-length-is-refused-at-the-limit
  (with-server [s (f/start! (fn [_ out]
                              (f/write! out "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n")
                              (let [chunk (byte-array 65536 (byte 97))]
                                (try (loop [] (f/write! out chunk) (recur))
                                     (catch Exception _ nil))))
                            {})]
    (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:max-bytes 100000}))))]
      (is (= ::http/too-large r))
      (is (< ms 5000)))))

(deftest a-body-of-exactly-the-limit-is-kept-and-one-byte-more-is-refused
  (let [body (str/join (repeat 1000 "a"))
        unlength (fn [b] (fn [_ out]
                           (f/write! out "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n")
                           (f/write! out b)))]
    (with-server [s (f/start! (ok-handler body) {})]
      (is (= 1000 (alength ^bytes (:body (http/request :get (:base s) {:max-bytes 1000}))))))
    (with-server [s (f/start! (unlength body) {})]
      (is (= 1000 (count (:body (http/request :get (:base s) {:max-bytes 1000 :as :text}))))))
    (with-server [s (f/start! (ok-handler (str body "a")) {})]
      (is (= ::http/too-large (refusal #(http/request :get (:base s) {:max-bytes 1000})))))
    (with-server [s (f/start! (unlength (str body "a")) {})]
      (is (= ::http/too-large (refusal #(http/request :get (:base s) {:max-bytes 1000})))))))

;; Deadlines

(deftest a-server-that-never-answers-is-cut-off-at-the-deadline
  (with-server [s (f/start! (fn [_ _] (Thread/sleep 5000)) {})]
    (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:timeout-ms 500}))))]
      (is (= ::http/timeout r))
      (is (<= 450 ms 2500) (str ms " ms")))))

(deftest a-trickled-body-is-cut-off-at-the-deadline
  (let [trickle (fn [n] (fn [_ out]
                          (f/write! out (str "HTTP/1.1 200 OK\r\nContent-Length: " n "\r\n\r\n"))
                          (try (dotimes [_ n] (f/write! out "a") (Thread/sleep 100))
                               (catch Exception _ nil))))]
    (with-server [s (f/start! (trickle 1000) {})]
      (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) {:timeout-ms 800}))))]
        (is (= ::http/timeout r))
        (is (<= 750 ms 2800) (str ms " ms"))))
    (testing "control: the same trickle inside the deadline completes"
      (with-server [s (f/start! (trickle 5) {})]
        (is (= "aaaaa" (:body (http/request :get (:base s) {:timeout-ms 5000 :as :text}))))))))

(deftest a-stalled-tls-handshake-is-cut-off-by-the-shorter-bound
  (with-server [s (f/start! (ok-handler "") {:tls? true :stall-handshake? true})]
    (doseq [[opts expected] [[{:connect-timeout-ms 300 :timeout-ms 5000} ::http/connect-timeout]
                             [{:connect-timeout-ms 5000 :timeout-ms 300} ::http/timeout]]]
      (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s) (assoc opts :ssl-context (trust))))))]
        (is (= expected r) (pr-str opts))
        (is (<= 250 ms 2500) (str ms " ms"))))))

(deftest the-deadline-covers-every-redirect
  (with-server [s (f/start! (fn [_ out]
                              (Thread/sleep 300)
                              (f/respond! out 302 {"Location" "/again"} ""))
                            {})]
    (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get (:base s)
                                                            {:timeout-ms 1000 :max-redirects 20}))))]
      (is (= ::http/timeout r))
      (is (< ms 2500) (str ms " ms")))))

;; Redirects

(deftest redirects-are-not-followed-by-default
  (with-server [s (f/start! (redirect-handler 302 "/final") {})]
    (let [r (http/request :get (:base s))]
      (is (= 302 (:status r)))
      (is (= "/final" (http/header r "location")))
      (is (= 1 (count @(:requests s)))))))

(deftest a-same-origin-redirect-keeps-the-credentials
  (with-server [s (f/start! (redirect-handler 302 "/final") {})]
    (let [r (http/request :get (:base s) {:max-redirects 1 :as :text
                                          :headers {"Authorization" (str "Bearer " marker)}})]
      (is (= "final" (:body r)))
      (is (= (str (:base s) "/final") (:url r)))
      (is (= [(str "Bearer " marker) (str "Bearer " marker)]
             (map #(get-in % [:headers "authorization"]) @(:requests s)))))))

(deftest a-cross-origin-redirect-drops-the-credentials
  (with-server [b (f/start! (ok-handler "elsewhere") {})]
    (with-server [a (f/start! (redirect-handler 307 (str (:base b) "/final")) {})]
      (let [r (http/request :get (:base a) {:max-redirects 1 :as :text
                                            :headers {"Authorization" (str "Bearer " marker)
                                                      "Proxy-Authorization" "Basic x"
                                                      "Cookie" "session=1"
                                                      "X-Keep" "kept"}})
            seen (:headers (first @(:requests b)))]
        (is (= "elsewhere" (:body r)))
        (is (= (str "Bearer " marker) (get-in (first @(:requests a)) [:headers "authorization"]))
            "control: the first hop carried it")
        (is (not (contains? seen "authorization")))
        (is (not (contains? seen "proxy-authorization")))
        (is (not (contains? seen "cookie")))
        (is (= "kept" (get seen "x-keep")))))))

(deftest a-redirect-from-https-to-http-is-refused
  (with-server [plain (f/start! (ok-handler "plain") {})]
    (with-server [tls (f/start! (redirect-handler 302 (str (:base plain) "/final")) {:tls? true})]
      (is (= ::http/redirect-refused
             (refusal #(http/request :get (:base tls) {:ssl-context (trust) :max-redirects 3
                                                       :allow-insecure-http? true}))))
      (is (empty? @(:requests plain)) "the http server was never contacted"))))

(deftest a-redirect-from-http-to-https-is-followed
  (with-server [tls (f/start! (ok-handler "upgraded") {:tls? true})]
    (with-server [plain (f/start! (redirect-handler 301 (str (:base tls) "/")) {})]
      (is (= "upgraded" (:body (http/request :get (:base plain) {:ssl-context (trust) :max-redirects 1
                                                                 :as :text})))))))

(deftest too-many-redirects-are-refused
  (with-server [s (f/start! (redirect-handler 302 "/loop") {})]
    (is (= ::http/too-many-redirects (refusal #(http/request :get (:base s) {:max-redirects 3}))))
    (is (= 4 (count @(:requests s))))))

(deftest see-other-turns-a-post-into-a-get-and-307-keeps-it
  (with-server [s (f/start! (redirect-handler 303 "/final") {})]
    (http/request :post (:base s) {:body "payload" :headers {"Content-Type" "text/plain"} :max-redirects 1})
    (let [[first-req second-req] @(:requests s)]
      (is (= ["POST" "payload"] [(:method first-req) (:body first-req)]))
      (is (= ["GET" ""] [(:method second-req) (:body second-req)]))
      (is (not (contains? (:headers second-req) "content-type")))))
  (with-server [s (f/start! (redirect-handler 307 "/final") {})]
    (http/request :post (:base s) {:body "payload" :max-redirects 1})
    (is (= [["POST" "payload"] ["POST" "payload"]]
           (map (fn [r] [(:method r) (:body r)]) @(:requests s))))))

(deftest a-redirect-the-caller-refuses-is-not-followed
  (with-server [b (f/start! (ok-handler "") {})]
    (with-server [a (f/start! (redirect-handler 302 (str (:base b) "/")) {})]
      (is (= ::http/redirect-refused
             (refusal #(http/request :get (:base a) {:max-redirects 1
                                                     :allow-redirect? (fn [url] (str/starts-with? url (:base a)))}))))
      (is (empty? @(:requests b))))))

(deftest redirects-to-other-schemes-or-nowhere-are-refused
  (doseq [location ["ftp://127.0.0.1/x" "file:///etc/passwd" (str "http://user" ":" "pw" "@127.0.0.1/") "http://example.invalid/"]]
    (with-server [s (f/start! (redirect-handler 302 location) {})]
      (is (contains? #{::http/redirect-refused ::http/insecure-url}
                     (refusal #(http/request :get (:base s) {:max-redirects 1})))
          location)))
  (with-server [s (f/start! (fn [_ out] (f/respond! out 302 "")) {})]
    (is (= ::http/redirect-refused (refusal #(http/request :get (:base s) {:max-redirects 1}))))))

;; Secrets

(deftest no-refusal-carries-the-credential
  (let [auth {"Authorization" (str "Bearer " marker)}
        clean? (fn [^Throwable e]
                 (and e
                      (http/refusal? e)
                      (not-any? (fn [^Throwable t]
                                  (str/includes? (str (.getMessage t) (pr-str (ex-data t))) marker))
                                (take-while some? (iterate #(.getCause ^Throwable %) e)))))
        q (str "?access_token=" marker)]
    (testing "insecure url"
      (is (clean? (caught #(http/request :get (str "http://example.invalid/x" q) {:headers auth})))))
    (testing "header injection"
      (is (clean? (caught #(http/request :get "https://127.0.0.1:1/" {:headers {"X-A" (str marker "\r\nX: 1")}})))))
    (testing "connection refused"
      (is (clean? (caught #(http/request :get (str "http://127.0.0.1:" (free-port) "/" q) {:headers auth})))))
    (testing "tls failure"
      (with-server [s (f/start! (ok-handler "") {:tls? true})]
        (is (clean? (caught #(http/request :get (str (:base s) q) {:headers auth}))))))
    (testing "timeout"
      (with-server [s (f/start! (fn [_ _] (Thread/sleep 3000)) {})]
        (is (clean? (caught #(http/request :get (str (:base s) q) {:headers auth :timeout-ms 300}))))))
    (testing "too large, with the credential echoed in the body"
      (with-server [s (f/start! (ok-handler (str "echo " marker)) {})]
        (is (clean? (caught #(http/request :get (str (:base s) q) {:headers auth :max-bytes 10}))))))
    (testing "redirect refused"
      (with-server [s (f/start! (redirect-handler 302 (str "ftp://127.0.0.1/" q)) {})]
        (is (clean? (caught #(http/request :get (str (:base s) q) {:headers auth :max-redirects 1}))))))
    (testing "control: the marker check sees a credential where one is"
      (is (not (clean? (ex-info (str "leak " marker) {:type ::http/failed})))))))
