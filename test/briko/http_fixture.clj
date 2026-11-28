;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http-fixture
  "Loopback HTTP and HTTPS servers for the briko.http tests, written on
   plain sockets so that a test controls every byte and every pause: the
   JDK's HttpServer is not available in babashka. The HTTPS server
   presents the self-signed certificate in http_server.p12 (CN=localhost,
   SANs localhost, 127.0.0.1 and ::1, valid until 2126); http_server.pem
   holds the same certificate for the client's trust. With {:cert :forge}
   it presents http_forge.p12 instead (CN and only SAN forge.briko.test,
   valid until 2126, certificate in http_forge.pem), a name no resolver
   answers, so a request for it can only arrive through :connect-to."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream DataInputStream InputStream OutputStream)
           (java.net InetAddress ServerSocket Socket)
           (java.nio.charset StandardCharsets)
           (java.security KeyStore)
           (javax.net.ssl KeyManagerFactory SSLContext SSLSocket)))

(set! *warn-on-reflection* true)

(def keystore-password "briko-test")

(def ^:private cert-files
  {:localhost "test/briko/http_server"
   :forge "test/briko/http_forge"})

(defn certificate-pem
  "The PEM text of the certificate a test server presents, by default the
   one for localhost."
  ([] (certificate-pem :localhost))
  ([cert] (slurp (io/file (str (cert-files cert) ".pem")))))

(defn- server-context
  ^SSLContext [cert]
  (let [password (.toCharArray ^String keystore-password)
        store (with-open [in (io/input-stream (io/file (str (cert-files cert) ".p12")))]
                (doto (KeyStore/getInstance "PKCS12") (.load in password)))
        kmf (doto (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
              (.init store password))]
    (doto (SSLContext/getInstance "TLS")
      (.init (.getKeyManagers kmf) nil nil))))

(defn- read-head
  "The request line and headers up to the blank line, as a string, or nil
   when the connection ends first."
  [^InputStream in]
  (let [out (ByteArrayOutputStream.)]
    (loop [last4 0]
      (let [b (.read in)]
        (cond
          (neg? b) nil
          :else (let [last4 (bit-and 0xFFFFFFFF (bit-or (bit-shift-left last4 8) b))]
                  (.write out b)
                  (if (= last4 0x0D0A0D0A)
                    (.toString out "ISO-8859-1")
                    (recur last4))))))))

(defn- parse-request
  [^InputStream in]
  (when-let [head (read-head in)]
    (let [[line & header-lines] (str/split-lines head)
          [method path] (str/split line #" ")
          headers (into {}
                        (for [l header-lines
                              :let [i (str/index-of l ":")]
                              :when i]
                          [(str/lower-case (subs l 0 i)) (str/trim (subs l (inc i)))]))
          n (some-> (get headers "content-length") parse-long)
          body (if (and n (pos? (long n)))
                 (let [bs (byte-array n)]
                   (loop [off 0]
                     (when (< off (long n))
                       (let [r (.read in bs off (- (long n) off))]
                         (when (pos? r) (recur (+ off r))))))
                   bs)
                 (byte-array 0))]
      {:method method :path path :headers headers :body (String. ^bytes body StandardCharsets/UTF_8)})))

(defn write!
  "Writes s (ISO-8859-1) or bytes to out and flushes."
  [^OutputStream out x]
  (.write out (if (string? x) (.getBytes ^String x StandardCharsets/ISO_8859_1) ^bytes x))
  (.flush out))

(defn respond!
  "Writes a whole response with a Content-Length."
  ([out status body] (respond! out status {} body))
  ([out status headers ^String body]
   (let [bs (.getBytes body StandardCharsets/UTF_8)]
     (write! out (str "HTTP/1.1 " status " X\r\n"
                      (str/join (for [[k v] headers] (str k ": " v "\r\n")))
                      "Content-Length: " (alength bs) "\r\nConnection: close\r\n\r\n"))
     (write! out bs))))

(defn- read-client-hello
  "The first TLS record the client sends, as bytes, and the SNI host name
   its ClientHello asks for, or nil: {:consumed :sni}. babashka exposes
   neither ExtendedSSLSession nor SNIMatcher, so the server reads the name
   itself before the handshake, from a ClientHello in one record."
  [^InputStream in]
  (let [din (DataInputStream. in)
        header (byte-array 5)
        _ (.readFully din header)
        n (bit-or (bit-shift-left (bit-and 0xFF (aget header 3)) 8) (bit-and 0xFF (aget header 4)))
        body (byte-array n)
        _ (.readFully din body)
        u8 (fn [i] (bit-and 0xFF (aget body (int i))))
        u16 (fn [i] (bit-or (bit-shift-left (u8 i) 8) (u8 (inc (long i)))))
        sni (try
              (let [;; handshake type 1, length 3, version 2, random 32
                    i 38
                    i (+ i 1 (u8 i))   ; session id
                    i (+ i 2 (u16 i))  ; cipher suites
                    i (+ i 1 (u8 i))   ; compression methods
                    end (+ i 2 (u16 i))]
                (loop [i (+ i 2)]
                  (when (< i end)
                    (let [type (u16 i)
                          len (u16 (+ i 2))]
                      (if (zero? type)
                        ;; server_name_list length 2, name type 1, name length 2
                        (String. body (int (+ i 9)) (int (u16 (+ i 7))) StandardCharsets/US_ASCII)
                        (recur (+ i 4 len)))))))
              (catch Exception _ nil))
        out (ByteArrayOutputStream.)]
    (.write out header)
    (.write out body)
    {:consumed (.toByteArray out) :sni sni}))

(defn start!
  "Starts a server on a free loopback port and returns {:port :base
   :requests :stop}. handler is called with the parsed request {:method
   :path :headers :body} and the connection's OutputStream, on a thread
   per connection; every request is also appended to the :requests atom,
   over TLS with the SNI names the client sent as :sni. Options: :tls?
   for HTTPS, :cert for its certificate (:localhost or :forge), :bind for
   the address (127.0.0.1), :stall-handshake? to accept connections and
   never answer the TLS handshake, and :linger-zero? to end every
   connection with a reset instead of a FIN."
  [handler {:keys [tls? cert ^String bind stall-handshake? linger-zero?] :or {bind "127.0.0.1" cert :localhost}}]
  (let [server (ServerSocket. 0 50 (InetAddress/getByName bind))
        ctx (when tls? (server-context cert))
        sockets (atom [])
        requests (atom [])
        serve (fn [^Socket plain]
                (try
                  (let [hello (when ctx (read-client-hello (.getInputStream plain)))
                        ^Socket s (if ctx
                                    (doto ^SSLSocket (.createSocket (.getSocketFactory ^SSLContext ctx)
                                                                    plain
                                                                    (ByteArrayInputStream. ^bytes (:consumed hello))
                                                                    true)
                                      (.setUseClientMode false))
                                    plain)]
                    (swap! sockets conj s)
                    (when-let [req (cond-> (parse-request (.getInputStream s))
                                     ctx (assoc :sni (:sni hello)))]
                      (swap! requests conj req)
                      (handler req (.getOutputStream s))))
                  (catch Exception _ nil)
                  (finally
                    (try (.close plain) (catch Exception _ nil)))))]
    (future
      (loop []
        (when-let [^Socket s (try (.accept server) (catch Exception _ nil))]
          (when linger-zero? (.setSoLinger s true 0))
          (swap! sockets conj s)
          (when-not stall-handshake?
            (future (serve s)))
          (recur))))
    {:port (.getLocalPort server)
     :base (str (if tls? "https" "http") "://" bind ":" (.getLocalPort server))
     :requests requests
     :stop (fn []
             (try (.close server) (catch Exception _ nil))
             (doseq [^Socket s @sockets]
               (try (.close s) (catch Exception _ nil))))}))

(defmacro with-server
  "Runs body with sym bound to the server start-expr starts, stopped
   afterwards."
  [[sym start-expr] & body]
  `(let [~sym ~start-expr]
     (try
       (let [res# (do ~@body)] res#)
       (finally ((:stop ~sym))))))
