;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.http
  "Makes one HTTP request and comes back within a deadline, with a body of
   bounded size, or refuses.

   `request` returns {:status :headers :body :url}: :headers maps each
   lower-cased response header name to the vector of its values, :body is
   a byte array, or a string with {:as :text}, and :url is the URL that
   answered, which differs from the one asked for only after a redirect.
   Every status is an answer, 4xx and 5xx included; `ok?` tells a 2xx.

   Every request is bounded:

     - :connect-timeout-ms bounds opening the connection, including the
       TLS handshake, and :timeout-ms the whole request: connecting,
       sending, every redirect, the response headers and the last byte of
       the body. A server that answers the headers and then sends the body
       one byte at a time is cut off at :timeout-ms like one that never
       answers. java.net.http applies its own request timeout only until
       the headers arrive, so the body is read on a thread of its own and
       abandoned at the deadline.
     - The body is read into memory up to :max-bytes. A response whose
       Content-Length exceeds the limit is refused before its body is
       read, and one without a length is refused as soon as it passes the
       limit; a body is never cut short and returned.
     - The URL must be https, or http to a loopback host: `localhost`, an
       IPv4 address in 127.0.0.0/8 or `[::1]`. Host names other than
       `localhost` are never resolved to decide this. Plain http to any
       other host needs {:allow-insecure-http? true}. A URL carrying user
       info is refused, because the credential would travel in the clear
       in every log that records the URL.
     - Redirects are not followed unless :max-redirects is positive; a 3xx
       then comes back as the answer. A followed redirect is checked like
       the first URL, never goes from https to http, whatever
       :allow-insecure-http? says, and drops Authorization,
       Proxy-Authorization and Cookie when it leaves the origin (scheme,
       host and port) the request started at. :allow-redirect? is a
       predicate over each next URL for callers with an allowlist.
     - Request header names must be HTTP tokens and values strings without
       control characters, so no header can smuggle a second header or a
       second request. Headers the HTTP client manages itself (Host,
       Connection, Content-Length, Expect, Upgrade) are refused.

   The client speaks HTTP/1.1, uses the system's trust store unless
   :ssl-context supplies another (`trust-context` builds one that trusts
   only the given certificates) and is closed before `request` returns.

   A request that cannot complete throws an ex-info whose data carries a
   stable :type, one of `refusal-types`, plus :url with the scheme, host,
   port and path of the URL concerned. Query strings, fragments, user
   info, header values and response bodies never appear in a refusal's
   message or data, so a token in any of them stays out of logs."
  (:require [clojure.string :as str])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream InputStream)
           (java.net URI URLEncoder)
           (java.net.http HttpClient HttpClient$Builder HttpClient$Redirect HttpClient$Version
                          HttpHeaders HttpRequest HttpRequest$BodyPublishers HttpRequest$Builder
                          HttpResponse HttpResponse$BodyHandlers HttpTimeoutException)
           (java.nio.charset StandardCharsets)
           (java.security KeyStore)
           (java.security.cert CertificateFactory)
           (java.time Duration)
           (java.util Optional)
           (javax.net.ssl SSLContext TrustManagerFactory)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a request uses where the caller gives none.

   - :headers, a map from header name to string value: none.
   - :query, a map from parameter name to a value or a sequence of values,
     appended to the URL's query string: none.
   - :body, a string (sent as UTF-8) or a byte array: none. Refused for
     :get and :head.
   - :as, the type of the response body: :bytes, or :text for a string
     decoded as UTF-8, malformed bytes replaced.
   - :connect-timeout-ms, the longest wait for a connection: 10 s.
   - :timeout-ms, the longest wait for the whole request: 60 s.
   - :max-bytes, the largest response body: 16 MiB.
   - :max-redirects, how many redirects to follow, at most 20: none.
   - :allow-redirect?, a predicate over the next URL string of a
     redirect, refusing it when false: none.
   - :allow-insecure-http?, whether plain http may go to a host that is
     not loopback: false.
   - :ssl-context, the javax.net.ssl.SSLContext for https: the system's."
  {:headers {}
   :query nil
   :body nil
   :as :bytes
   :connect-timeout-ms 10000
   :timeout-ms 60000
   :max-bytes 16777216
   :max-redirects 0
   :allow-redirect? nil
   :allow-insecure-http? false
   :ssl-context nil})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-url ::insecure-url ::invalid-header ::invalid-trust
    ::connect-failed ::connect-timeout ::tls-failed ::timeout ::too-large
    ::too-many-redirects ::redirect-refused ::failed ::interrupted})

(defn- refuse!
  ([type message] (refuse! type message {} nil))
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

;; URLs

(defn- redacted
  "uri as scheme, host, port and path: what a refusal may name. Query,
   fragment and user info are where tokens travel, so they are left out."
  [^URI uri]
  (str (.getScheme uri) "://" (.getHost uri)
       (when (pos? (.getPort uri)) (str ":" (.getPort uri)))
       (.getRawPath uri)))

(defn- ipv4-literal?
  [^String host]
  (let [parts (str/split host #"\." -1)]
    (and (= 4 (count parts))
         (every? (fn [^String p]
                   (and (<= 1 (count p) 3)
                        (every? #(Character/isDigit (char %)) p)
                        (<= (parse-long p) 255)))
                 parts))))

(defn- ipv6-groups
  "The eight 16-bit groups of an IPv6 literal without brackets, or nil
   for anything else, an embedded IPv4 address and a zone included."
  [^String s]
  (let [hex? (fn [^String g] (and (<= 1 (count g) 4)
                                  (every? #(<= 0 (Character/digit (char %) 16)) g)))
        groups (fn [^String part] (if (= "" part) [] (str/split part #":" -1)))
        [head tail & more] (str/split s #"::" -1)]
    (when (and (nil? more) (every? hex? (concat (groups head) (groups (or tail "")))))
      (let [h (groups head)
            t (groups (or tail ""))
            missing (- 8 (count h) (count t))]
        (when (if tail (pos? missing) (zero? missing))
          (mapv #(Long/parseLong % 16) (concat h (repeat missing "0") t)))))))

(defn- loopback?
  "Whether host names this machine without asking a resolver: localhost,
   127.0.0.0/8 or [::1]."
  [^String host]
  (let [host (str/lower-case host)]
    (boolean
     (or (= "localhost" host)
         (and (ipv4-literal? host) (str/starts-with? host "127."))
         (and (str/starts-with? host "[") (str/ends-with? host "]")
              (= [0 0 0 0 0 0 0 1] (ipv6-groups (subs host 1 (dec (count host))))))))))

(defn- parse-url
  "The URI of url, refusing anything but an absolute http or https URL
   with a host and no user info. type is the refusal's :type."
  ^URI [url type]
  (let [^URI uri (try (URI. (str url))
                      (catch Exception _
                        (refuse! type "the URL is not a valid URI" {})))
        scheme (some-> (.getScheme uri) str/lower-case)]
    (when-not (and (#{"http" "https"} scheme) (seq (.getHost uri)))
      (refuse! type "the URL is not an absolute http or https URL with a host"
               (if (and scheme (.getHost uri)) {:url (redacted uri)} {})))
    (when (.getRawUserInfo uri)
      (refuse! type "the URL carries user info; pass credentials in a header" {:url (redacted uri)}))
    uri))

(defn- https?
  [^URI uri]
  (= "https" (str/lower-case (.getScheme uri))))

(defn- check-transport!
  [^URI uri allow-insecure-http?]
  (when-not (or (https? uri) allow-insecure-http? (loopback? (.getHost uri)))
    (refuse! ::insecure-url
             "plain http is refused for a host that is not loopback; use https or pass :allow-insecure-http? true"
             {:url (redacted uri)})))

(defn- origin
  [^URI uri]
  [(str/lower-case (.getScheme uri))
   (str/lower-case (.getHost uri))
   (let [port (.getPort uri)]
     (if (neg? port) (if (https? uri) 443 80) port))])

(defn- encode
  ^String [x]
  (URLEncoder/encode (if (keyword? x) (name x) (str x)) "UTF-8"))

(defn- with-query
  "url with the parameters of query appended to its query string."
  [url query]
  (if (empty? query)
    (str url)
    (let [pairs (for [[k v] query
                      value (if (sequential? v) v [v])]
                  (str (encode k) "=" (encode value)))
          [base fragment] (str/split (str url) #"#" 2)]
      (str base (if (str/includes? base "?") "&" "?") (str/join "&" pairs)
           (when fragment (str "#" fragment))))))

;; Headers

(def ^:private token-chars
  (set "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def ^:private managed-headers
  #{"connection" "content-length" "expect" "host" "upgrade"})

(def ^:private cross-origin-headers
  #{"authorization" "proxy-authorization" "cookie"})

(defn- header-value?
  [v]
  (and (string? v)
       (every? (fn [c] (let [c (int c)] (or (= 9 c) (and (<= 32 c) (not= 127 c))))) v)))

(defn- check-headers!
  "headers as [name value] pairs, refusing an invalid name, an invalid
   value, a header the client manages and a name given twice. A refusal
   names the header, never its value."
  [headers]
  (when-not (map? headers)
    (refuse! ::invalid-options ":headers must be a map from name to string" {:option :headers}))
  (let [named (fn [k] (if (keyword? k) (name k) k))
        pairs (mapv (fn [[k v]] [(named k) v]) headers)]
    (doseq [[k v] pairs]
      (when-not (and (string? k) (seq k) (every? token-chars k))
        (refuse! ::invalid-header "a header name is not an HTTP token"
                 (if (string? k) {:header (str/join (filter token-chars k))} {})))
      (when (managed-headers (str/lower-case k))
        (refuse! ::invalid-header (str "the header " k " is set by the HTTP client") {:header k}))
      (when-not (header-value? v)
        (refuse! ::invalid-header
                 (str "the value of the header " k " is not a string free of control characters")
                 {:header k})))
    (when-let [twice (seq (for [[n c] (frequencies (map (comp str/lower-case first) pairs))
                                :when (< 1 c)]
                            n))]
      (refuse! ::invalid-header (str "headers given twice: " (pr-str (vec twice)))
               {:header (first twice)}))
    pairs))

(defn- without-headers
  [pairs names]
  (vec (remove (fn [[k]] (names (str/lower-case k))) pairs)))

;; Options

(defn- pos-int-below?
  [limit x]
  (and (int? x) (pos? (long x)) (< (long x) (long limit))))

(def ^:private method-names
  {:get "GET" :head "HEAD" :post "POST" :put "PUT" :patch "PATCH" :delete "DELETE"
   :options "OPTIONS"})

(def ^:private option-checks
  {:as [#{:bytes :text} "one of :bytes and :text"]
   :connect-timeout-ms [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   :timeout-ms [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   ;; the body is held in one array
   :max-bytes [(partial pos-int-below? (- Integer/MAX_VALUE 8)) "a positive int below Integer/MAX_VALUE"]
   :max-redirects [(fn [n] (and (int? n) (<= 0 (long n) 20))) "an int from 0 to 20"]
   :allow-redirect? [(fn [p] (or (nil? p) (ifn? p))) "nil or a predicate"]
   :allow-insecure-http? [boolean? "a boolean"]
   :ssl-context [(fn [c] (or (nil? c) (instance? SSLContext c))) "nil or a javax.net.ssl.SSLContext"]
   :query [(fn [q] (or (nil? q) (map? q))) "nil or a map"]
   :body [(fn [b] (or (nil? b) (string? b) (bytes? b))) "nil, a string or a byte array"]})

(defn- options
  [method opts]
  (when-not (contains? method-names method)
    (refuse! ::invalid-options (str "the method must be one of " (pr-str (sort (keys method-names))))
             {:option :method}))
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k})))
    (when (and (:body opts) (#{:get :head} method))
      (refuse! ::invalid-options (str "a " (method-names method) " request carries no body") {:option :body}))
    opts))

(defn trust-context
  "An SSLContext that trusts exactly the X.509 certificates in pem, a
   string or byte array of one or more PEM blocks, and nothing from the
   system's trust store. For a private certificate authority or a test
   server's self-signed certificate."
  ^SSLContext [pem]
  (let [bs (cond (string? pem) (.getBytes ^String pem StandardCharsets/UTF_8)
                 (bytes? pem) pem
                 :else (refuse! ::invalid-trust "the certificates must be a string or byte array" {}))
        certs (try (vec (.generateCertificates (CertificateFactory/getInstance "X.509")
                                               (ByteArrayInputStream. ^bytes bs)))
                   (catch Exception e
                     (refuse! ::invalid-trust "the certificates are not valid PEM X.509 certificates" {} e)))
        _ (when (empty? certs)
            (refuse! ::invalid-trust "no certificate found" {}))
        store (doto (KeyStore/getInstance (KeyStore/getDefaultType)) (.load nil nil))]
    (doseq [[i cert] (map-indexed vector certs)]
      (.setCertificateEntry store (str "trusted-" i) cert))
    (let [tmf (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
                (.init store))]
      (doto (SSLContext/getInstance "TLS")
        (.init nil (.getTrustManagers tmf) nil)))))

;; Deadlines

(defn- remaining-ms
  ^long [^long deadline]
  (quot (- deadline (System/nanoTime)) 1000000))

(defn- timed-out!
  [^URI uri timeout-ms]
  (refuse! ::timeout (format "the request did not complete within %d ms" timeout-ms)
           {:url (redacted uri) :limit timeout-ms}))

;; Sending

(defn- client
  ^HttpClient [{:keys [connect-timeout-ms ssl-context]}]
  (let [^HttpClient$Builder b (-> (HttpClient/newBuilder)
                                  (.connectTimeout (Duration/ofMillis connect-timeout-ms))
                                  (.followRedirects HttpClient$Redirect/NEVER)
                                  (.version HttpClient$Version/HTTP_1_1))]
    (when ssl-context
      (.sslContext b ^SSLContext ssl-context))
    (.build b)))

(defn- causes
  [^Throwable e]
  (take-while some? (iterate #(.getCause ^Throwable %) e)))

(defn- class-named?
  [prefix ^Throwable e]
  (str/starts-with? (.getName (class e)) prefix))

(defn- send-failure!
  "Refuses the exception e that sending to uri threw, by what it says
   about the connection. JDK messages carry no request headers."
  [^Throwable e ^URI uri {:keys [connect-timeout-ms timeout-ms remaining]}]
  (let [chain (causes e)
        data {:url (redacted uri)}]
    (cond
      ;; java.net.http reports the request timeout firing while it connects
      ;; as a connect timeout too; it is the deadline whenever that was the
      ;; shorter bound
      (and (some (partial class-named? "java.net.http.HttpConnectTimeoutException") chain)
           (or (nil? remaining) (< (long connect-timeout-ms) (long remaining))))
      (refuse! ::connect-timeout (format "connecting took longer than %d ms" connect-timeout-ms)
               (assoc data :limit connect-timeout-ms) e)

      (some #(instance? HttpTimeoutException %) chain)
      (refuse! ::timeout (format "the request did not complete within %d ms" timeout-ms)
               (assoc data :limit timeout-ms) e)

      (some (partial class-named? "javax.net.ssl.") chain)
      (refuse! ::tls-failed "the TLS handshake failed" data e)

      (some #(instance? java.net.ConnectException %) chain)
      (refuse! ::connect-failed "the connection could not be opened" data e)

      :else
      (refuse! ::failed (str "the request failed: " (.getSimpleName (class e))) data e))))

(defn- http-request
  ^HttpRequest [^URI uri method headers body remaining]
  (let [^HttpRequest$Builder b (-> (HttpRequest/newBuilder uri)
                                   (.timeout (Duration/ofMillis remaining)))]
    (doseq [[k v] headers]
      (.header b ^String k ^String v))
    (case method
      :get (.GET b)
      :head (.method b "HEAD" (HttpRequest$BodyPublishers/noBody))
      (.method b ^String (method-names method)
               (if body
                 (HttpRequest$BodyPublishers/ofByteArray ^bytes body)
                 (HttpRequest$BodyPublishers/noBody))))
    (.build b)))

(defn- send!
  ^HttpResponse [^HttpClient c ^URI uri method headers body deadline opts]
  (let [remaining (remaining-ms deadline)]
    (when-not (pos? remaining)
      (timed-out! uri (:timeout-ms opts)))
    (let [req (try (http-request uri method headers body remaining)
                   (catch IllegalArgumentException _
                     ;; the client's own header checks; their message quotes
                     ;; the value, so it is not kept as the cause
                     (refuse! ::invalid-header "the HTTP client refused a header" {:url (redacted uri)})))]
      (try
        (.send c req (HttpResponse$BodyHandlers/ofInputStream))
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (refuse! ::interrupted "interrupted while waiting for the response" {:url (redacted uri)} e))
        (catch java.io.IOException e
          (send-failure! e uri (assoc opts :remaining remaining)))))))

(defn- response-headers
  [^HttpResponse resp]
  (into (sorted-map)
        (for [[k vs] (.map ^HttpHeaders (.headers resp))
              :when (not (str/starts-with? (str k) ":"))]
          [(str/lower-case (str k)) (vec vs)])))

(defn- close-quietly
  [^java.io.Closeable c]
  (try (.close c) (catch Exception _ nil)))

(defn- read-limited
  "Reads in to its end, keeping at most max-bytes. Returns {:bytes b},
   {:too-large true} or {:error e}."
  [^InputStream in max-bytes]
  (try
    (let [out (ByteArrayOutputStream.)
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (cond
            (neg? n) {:bytes (.toByteArray out)}
            (< (long max-bytes) (+ (.size out) n)) {:too-large true}
            :else (do (.write out buffer 0 n) (recur))))))
    (catch Exception e {:error e})))

(defn- too-large!
  [^URI uri max-bytes]
  (refuse! ::too-large (format "the response body is larger than %d bytes" max-bytes)
           {:url (redacted uri) :limit max-bytes}))

(defn- read-body
  "The body of resp as bytes, within max-bytes and the deadline. The read
   runs on a thread of its own so that a server trickling the body is cut
   off at the deadline; the stream is closed whatever happens."
  ^bytes [^HttpResponse resp ^URI uri deadline {:keys [max-bytes timeout-ms]}]
  (let [^InputStream in (.body resp)
        declared (or (some-> ^Optional (.firstValue ^HttpHeaders (.headers resp) "content-length")
                             (.orElse nil) str/trim parse-long)
                     -1)]
    (when (< (long max-bytes) declared)
      (close-quietly in)
      (too-large! uri max-bytes))
    (let [reading (future (read-limited in max-bytes))
          result (try (deref reading (max 0 (remaining-ms deadline)) ::timed-out)
                      (catch InterruptedException e
                        (.interrupt (Thread/currentThread))
                        (refuse! ::interrupted "interrupted while reading the response" {:url (redacted uri)} e))
                      (finally
                        (when-not (realized? reading) (future-cancel reading))
                        (close-quietly in)))]
      (cond
        (= ::timed-out result) (timed-out! uri timeout-ms)
        (:too-large result) (too-large! uri max-bytes)
        (:error result) (send-failure! (:error result) uri {:timeout-ms timeout-ms})
        :else (:bytes result)))))

;; Redirects

(def ^:private redirect-statuses #{301 302 303 307 308})

(defn- next-hop
  "The request a redirect from uri to location asks for, as {:uri
   :method :headers :body}, refusing a redirect the rules forbid."
  [^URI uri location {:keys [method headers body]} start-origin
   {:keys [allow-insecure-http? allow-redirect?]} status]
  (when-not location
    (refuse! ::redirect-refused "the redirect names no location" {:url (redacted uri)}))
  (let [^URI target (parse-url (try (str (.resolve uri ^String location))
                                    (catch Exception _
                                      (refuse! ::redirect-refused "the redirect location is not a valid URI"
                                               {:url (redacted uri)})))
                               ::redirect-refused)]
    (when (and (https? uri) (not (https? target)))
      (refuse! ::redirect-refused "a redirect from https to http is refused"
               {:url (redacted uri) :location (redacted target)}))
    (check-transport! target allow-insecure-http?)
    (when (and allow-redirect? (not (allow-redirect? (str target))))
      (refuse! ::redirect-refused "the redirect was refused by :allow-redirect?"
               {:url (redacted uri) :location (redacted target)}))
    (let [to-get? (and (not= :head method)
                       (or (= 303 status) (and (#{301 302} status) (= :post method))))
          headers (cond-> headers
                    (not= start-origin (origin target)) (without-headers cross-origin-headers)
                    to-get? (without-headers #{"content-type"}))]
      {:uri target
       :method (if to-get? :get method)
       :headers headers
       :body (when-not to-get? body)})))

(defn request
  "Sends one HTTP request for method (:get :head :post :put :patch
   :delete :options) to url and returns {:status :headers :body :url}.
   Options and their defaults are in `defaults`; see the namespace for the
   rules every request follows and `refusal-types` for its refusals."
  ([method url] (request method url nil))
  ([method url opts]
   (let [{:keys [timeout-ms max-redirects as] :as opts} (options method opts)
         headers (check-headers! (:headers opts))
         ^URI first-uri (parse-url (with-query url (:query opts)) ::invalid-url)
         _ (check-transport! first-uri (:allow-insecure-http? opts))
         body (let [b (:body opts)]
                (if (string? b) (.getBytes ^String b StandardCharsets/UTF_8) b))
         deadline (+ (System/nanoTime) (* 1000000 (long timeout-ms)))
         start-origin (origin first-uri)
         c (client opts)]
     (try
       (loop [hop {:uri first-uri :method method :headers headers :body body}
              followed 0]
         (let [^URI uri (:uri hop)
               resp (send! c uri (:method hop) (:headers hop) (:body hop) deadline opts)
               status (.statusCode resp)]
           (if (and (redirect-statuses status) (pos? (long max-redirects)))
             (let [location (.orElse ^Optional (.firstValue ^HttpHeaders (.headers resp) "location") nil)]
               (close-quietly ^InputStream (.body resp))
               (when (<= (long max-redirects) followed)
                 (refuse! ::too-many-redirects (format "more than %d redirects" max-redirects)
                          {:url (redacted uri) :limit max-redirects}))
               (recur (next-hop uri location hop start-origin opts status) (inc followed)))
             (let [bs (read-body resp uri deadline opts)]
               {:status status
                :headers (response-headers resp)
                :body (if (= :text as) (String. bs StandardCharsets/UTF_8) bs)
                :url (str uri)}))))
       (finally
         (.shutdownNow c))))))

(defn ok?
  "Whether response has a 2xx status."
  [response]
  (boolean (some-> (:status response) (#(<= 200 (long %) 299)))))

(defn header
  "The first value of the response header named name, any case, or nil."
  [response name]
  (first (get (:headers response) (str/lower-case name))))
