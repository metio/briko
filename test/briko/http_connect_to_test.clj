;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-connect-to-test
  "Requests for https://forge.briko.test, a name no resolver answers,
   reach the loopback test server only through :connect-to, so every
   answer below came through the tunnel."
  (:require [briko.http :as http]
            [briko.http-fixture :as f :refer [with-server]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.net InetAddress ServerSocket Socket URI)
           (java.nio.charset StandardCharsets)))

;; A value that stands for a credential; it must never reach a refusal.
(def ^:private marker "5d41402abc4b2a76b9719d911017c592ae2f8b1ad3c4e5f60718293a4b5c6d7e")

(def ^:private forge "https://forge.briko.test")

(defn- forge-trust []
  (http/trust-context (f/certificate-pem :forge)))

(defn- refusal
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

(defn- to
  [s]
  {:host "127.0.0.1" :port (:port s)})

(defn- free-port
  []
  (let [s (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))
        port (.getLocalPort s)]
    (.close s)
    port))

(defn- ok-handler
  [body]
  (fn [_ out] (f/respond! out 200 {"Content-Type" "text/plain"} body)))

(deftest a-request-reaches-the-forwarded-address-and-names-the-url-host
  (with-server [s (f/start! (ok-handler "through the tunnel") {:tls? true :cert :forge})]
    (let [r (http/request :post (str forge "/api?x=1")
                          {:connect-to (to s) :ssl-context (forge-trust) :as :text :body "payload"
                           :headers {"Authorization" (str "Bearer " marker)}})
          req (first @(:requests s))]
      (is (= 200 (:status r)))
      (is (= "through the tunnel" (:body r)))
      (is (= (str forge "/api?x=1") (:url r)))
      (is (= "forge.briko.test" (get-in req [:headers "host"])) "Host names the URL's host")
      (is (= "forge.briko.test" (:sni req)) "SNI names the URL's host")
      (is (= ["POST" "/api?x=1" "payload"] [(:method req) (:path req) (:body req)]))
      (is (= (str "Bearer " marker) (get-in req [:headers "authorization"]))))
    (testing "an explicit port in the URL is named the same way"
      (http/request :get (str forge ":8443/") {:connect-to (to s) :ssl-context (forge-trust)})
      (is (= "forge.briko.test:8443" (get-in (second @(:requests s)) [:headers "host"]))))))

(deftest tls-verifies-the-url-host-and-not-the-forwarded-address
  (with-server [s (f/start! (ok-handler "x") {:tls? true :cert :forge})]
    (testing "a URL host the certificate does not name"
      (is (= ::http/tls-failed (refusal #(http/request :get "https://other.briko.test/"
                                                       {:connect-to (to s) :ssl-context (forge-trust)})))))
    (testing "the forwarded address itself is not what is verified"
      (is (= ::http/tls-failed (refusal #(http/request :get "https://127.0.0.1/"
                                                       {:connect-to (to s) :ssl-context (forge-trust)})))))
    (testing "the system trust does not know the test certificate"
      (is (= ::http/tls-failed (refusal #(http/request :get forge {:connect-to (to s)})))))
    (is (empty? @(:requests s)) "no refused handshake reached the handler"))
  (with-server [s (f/start! (ok-handler "x") {:tls? true})]
    (testing "a server presenting the localhost certificate for forge.briko.test"
      (is (= ::http/tls-failed
             (refusal #(http/request :get forge {:connect-to (to s)
                                                 :ssl-context (http/trust-context (str (f/certificate-pem)
                                                                                       (f/certificate-pem :forge)))})))))
    (testing "control: the same server answers for the name in its certificate"
      (is (= 200 (:status (http/request :get "https://localhost/"
                                        {:connect-to (to s) :ssl-context (http/trust-context (f/certificate-pem))})))))))

(deftest connect-to-needs-https-and-a-valid-address
  (is (= ::http/invalid-options (refusal #(http/request :get "http://127.0.0.1:1/" {:connect-to {:host "127.0.0.1" :port 2}}))))
  (doseq [c [{:host "127.0.0.1"} {:port 1} {:host "127.0.0.1" :port 0} {:host "127.0.0.1" :port 65536}
             {:host "127.0.0.1" :port "443"} {:host "" :port 1} {:host "a b" :port 1} {:host "a:1" :port 1}
             {:host "127.0.0.1" :port 1 :extra true} "127.0.0.1:1" {:host 5 :port 1}]]
    (is (= ::http/invalid-options (refusal #(http/request :get forge {:connect-to c}))) (pr-str c))))

(deftest a-closed-forwarded-port-is-a-connection-failure
  (is (= ::http/connect-failed (refusal #(http/request :get forge {:connect-to {:host "127.0.0.1" :port (free-port)}
                                                                   :ssl-context (forge-trust)})))))

(deftest a-dial-that-times-out-is-a-connect-timeout
  ;; a real dial timeout needs an address that drops packets, which is
  ;; off this machine; the tunnel's record of the failure is set instead
  (with-redefs-fn {#'briko.http/dial! (fn [_ _ _ failure _] (reset! failure ::http/connect-timeout) nil)}
    (fn []
      (is (= ::http/connect-timeout (refusal #(http/request :get forge {:connect-to {:host "127.0.0.1" :port 1}
                                                                        :ssl-context (forge-trust)})))))))

(deftest an-ipv6-forwarded-address-is-dialled
  (let [s (try (f/start! (ok-handler "six") {:tls? true :cert :forge :bind "::1"})
               (catch Exception _ nil))]
    (if-not s
      (println "SKIPPED an-ipv6-forwarded-address-is-dialled: ::1 cannot be bound")
      (try
        (is (= "six" (:body (http/request :get forge {:connect-to {:host "[::1]" :port (:port s)}
                                                      :ssl-context (forge-trust) :as :text}))))
        (finally ((:stop s)))))))

(deftest a-stalled-handshake-through-the-tunnel-is-cut-off-at-the-deadline
  (with-server [s (f/start! (ok-handler "") {:tls? true :cert :forge :stall-handshake? true})]
    (doseq [opts [{:connect-timeout-ms 5000 :timeout-ms 400}
                  ;; the connect timeout bounds the dial, not the handshake
                  {:connect-timeout-ms 200 :timeout-ms 1200}]]
      (let [[r ms] (elapsed-ms #(refusal (fn [] (http/request :get forge (assoc opts :connect-to (to s)
                                                                                :ssl-context (forge-trust))))))]
        (is (= ::http/timeout r) (pr-str opts))
        (is (<= (- (long (:timeout-ms opts)) 50) ms (+ (long (:timeout-ms opts)) 2000)) (str ms " ms"))))))

(deftest a-same-origin-redirect-goes-through-the-tunnel-and-another-origin-is-refused
  (with-server [s (f/start! (fn [req out]
                              (cond
                                (str/starts-with? (:path req) "/final") (f/respond! out 200 "final")
                                (str/starts-with? (:path req) "/away") (f/respond! out 302 {"Location" "https://elsewhere.briko.test/"} "")
                                :else (f/respond! out 302 {"Location" "/final"} "")))
                            {:tls? true :cert :forge})]
    (is (= "final" (:body (http/request :get (str forge "/start") {:connect-to (to s) :ssl-context (forge-trust)
                                                                   :max-redirects 2 :as :text}))))
    (is (= ["/start" "/final"] (map :path @(:requests s))))
    (is (= ::http/redirect-refused (refusal #(http/request :get (str forge "/away") {:connect-to (to s) :ssl-context (forge-trust)
                                                                                     :max-redirects 2}))))
    (is (= ["/start" "/final" "/away"] (map :path @(:requests s))))
    (testing "another port of the same host is another origin"
      (with-server [p (f/start! (fn [_ out] (f/respond! out 302 {"Location" (str forge ":9443/")} "")) {:tls? true :cert :forge})]
        (is (= ::http/redirect-refused (refusal #(http/request :get forge {:connect-to (to p) :ssl-context (forge-trust)
                                                                           :max-redirects 2}))))))))

(deftest a-redirected-request-without-connect-to-is-unaffected
  (with-server [s (f/start! (fn [req out]
                              (if (str/starts-with? (:path req) "/final")
                                (f/respond! out 200 "final")
                                (f/respond! out 302 {"Location" "/final"} "")))
                            {})]
    (is (= "final" (:body (http/request :get (:base s) {:max-redirects 1 :as :text}))))))

;; The tunnel

(defn- read-head
  [^InputStream in]
  (let [out (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (when-not (neg? b)
          (.write out b)
          (when-not (str/ends-with? (.toString out "ISO-8859-1") "\r\n\r\n")
            (recur)))))
    (.toString out "ISO-8859-1")))

(defn- ask-tunnel
  "Sends head to the tunnel on port and returns the status line it answers."
  [port ^String head]
  (with-open [s (Socket. "127.0.0.1" (int port))]
    (.setSoTimeout s 5000)
    (.write (.getOutputStream s) (.getBytes head StandardCharsets/ISO_8859_1))
    (.flush (.getOutputStream s))
    (first (str/split-lines (read-head (.getInputStream s))))))

(deftest the-tunnel-admits-only-a-connect-to-the-url-authority
  (let [upstream (ServerSocket. 0 5 (InetAddress/getByName "127.0.0.1"))
        accepted (atom 0)
        _ (future (while (try (.accept upstream) (catch Exception _ nil))
                    (swap! accepted inc)))
        deadline (+ (System/nanoTime) 10000000000)
        tunnel (#'briko.http/start-tunnel! (URI. "https://forge.briko.test/x")
                                           {:host "127.0.0.1" :port (.getLocalPort upstream)}
                                           deadline {:connect-timeout-ms 5000})]
    (try
      (doseq [head ["CONNECT other.briko.test:443 HTTP/1.1\r\nHost: other.briko.test:443\r\n\r\n"
                    "CONNECT forge.briko.test:444 HTTP/1.1\r\n\r\n"
                    "CONNECT 127.0.0.1:443 HTTP/1.1\r\n\r\n"
                    "GET https://forge.briko.test/ HTTP/1.1\r\nHost: forge.briko.test\r\n\r\n"
                    "GET / HTTP/1.1\r\n\r\n"]]
        (is (str/starts-with? (ask-tunnel (:port tunnel) head) "HTTP/1.1 403") head))
      (Thread/sleep 200)
      (is (zero? @accepted) "nothing was dialled for a refused request")
      (testing "control: a CONNECT to the URL authority, any case, is joined"
        (is (str/starts-with? (ask-tunnel (:port tunnel) "CONNECT Forge.Briko.Test:443 HTTP/1.1\r\n\r\n") "HTTP/1.1 200"))
        (Thread/sleep 200)
        (is (= 1 @accepted)))
      (finally
        ((:close tunnel))
        (.close upstream)))))

(defn- listening?
  "Whether something accepts connections on the loopback port. A probe whose
  ephemeral source port equals the port it dials joins itself (a TCP
  self-connect) and succeeds although nothing listens, so that counts as not
  listening."
  [port]
  (try
    (with-open [s (Socket. "127.0.0.1" (int port))]
      (not= (.getLocalPort s) (int port)))
    (catch java.io.IOException _ false)))

(deftest listening-tells-a-live-listener-from-a-closed-port
  (let [server (ServerSocket. 0 8 (InetAddress/getByName "127.0.0.1"))
        port (.getLocalPort server)]
    (is (listening? port) "control: a live listener is open")
    (.close server)
    (is (not (listening? port)))))

(deftest a-closed-tunnel-accepts-no-connection
  ;; The race between close and the accept thread shows in a small share of
  ;; closes, so one close proves little; each round closes a tunnel whose
  ;; accept thread is blocked in accept.
  (let [listening (atom [])]
    (dotimes [_ 200]
      (let [tunnel (#'briko.http/start-tunnel! (URI. "https://forge.briko.test/") {:host "127.0.0.1" :port 1}
                                               (+ (System/nanoTime) 5000000000) {:connect-timeout-ms 1000})
            port (:port tunnel)]
        (try
          (Thread/sleep 2)
          ((:close tunnel))
          (when (listening? port) (swap! listening conj port))
          (finally ((:close tunnel))))))
    (is (empty? @listening))))

(deftest the-tunnel-is-closed-when-the-request-returns
  (with-server [s (f/start! (ok-handler "x") {:tls? true :cert :forge})]
    (let [start @#'briko.http/start-tunnel!
          ports (atom [])]
      (with-redefs-fn {#'briko.http/start-tunnel! (fn [& args]
                                                    (let [t (apply start args)]
                                                      (swap! ports conj (:port t))
                                                      t))}
        (fn []
          (is (= 200 (:status (http/request :get forge {:connect-to (to s) :ssl-context (forge-trust)}))))))
      (is (= 1 (count @ports)))
      (is (not (listening? (first @ports)))))))

;; Secrets

(deftest no-connect-to-refusal-carries-the-credential
  (with-server [s (f/start! (ok-handler "") {:tls? true :cert :forge})]
    (doseq [[why f] [["tls failure" #(http/request :get (str "https://other.briko.test/?t=" marker)
                                                   {:connect-to (to s) :ssl-context (forge-trust)
                                                    :headers {"Authorization" (str "Bearer " marker)}})]
                     ["closed port" #(http/request :get (str forge "/?t=" marker)
                                                   {:connect-to {:host "127.0.0.1" :port (free-port)}
                                                    :headers {"Authorization" (str "Bearer " marker)}})]
                     ["plain http" #(http/request :get (str "http://127.0.0.1/?t=" marker) {:connect-to (to s)})]]]
      (let [e (try (f) nil (catch Exception e e))]
        (is (http/refusal? e) why)
        (is (not-any? (fn [^Throwable t] (str/includes? (str (.getMessage t) (pr-str (ex-data t))) marker))
                      (take-while some? (iterate #(.getCause ^Throwable %) e)))
            why)))))
