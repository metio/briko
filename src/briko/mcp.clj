;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.mcp
  "The message layer of a Model Context Protocol server: one JSON-RPC
   message in, the response to it and the HTTP status it calls for out,
   whatever carries the bytes.

   Two eras of the protocol are answered side by side. The legacy
   revisions (`legacy-versions`) open with an `initialize` handshake. The
   modern revisions (`modern-versions`) have no handshake: every request
   names its revision and the client's capabilities in its own `_meta`,
   and is answered from what it carries alone. A message is modern when
   its `_meta` names a protocol version, when its method is
   `server/discover` (which only the modern revisions know), or when the
   HTTP request's `MCP-Protocol-Version` header names a modern revision;
   an `initialize` is always the handshake.

   A server is a map:

   - :server-info, `{:name … :version …}`, required.
   - :capabilities, the capabilities map both eras announce, required.
   - :instructions, the server instructions, left out when nil.
   - :discover, extra keys of the `server/discover` result, such as
     `{:ttlMs 300000 :cacheScope \"public\"}`.
   - :handlers, method name to `(fn [params request] result)`. `params`
     is the message's params (string keys, as parsed); `request` is
     `{:era :legacy|:modern :id … :method … :headers …}`, where
     :headers is nil on stdio. `ping` is answered without a handler, and
     `tool-handlers` builds `tools/list` and `tools/call` from a tool
     registry.

   A handler refuses a request with `rpc-error!`, which answers with a
   JSON-RPC error object instead of a result. Any other exception,
   an Error included, answers -32603, so the session survives whatever
   the client sends.

   Notifications (messages without an `id`) never get a response and run
   no handler. Over HTTP the caller answers 202 for one that was
   accepted; a modern notification that fails the `_meta` or header
   checks carries the status of the refusal.

   Messages are read and written by briko.json. A message is exactly one
   JSON value of RFC 8259 no longer than :max-message-bytes in UTF-8
   (16 MiB unless the caller sets it), nesting at most 64 levels, with no
   key named twice and no number literal over 100 characters; anything
   else answers -32700 with a message and :data naming the rule it broke,
   never the text. Answers are written with every hidden code point
   escaped as \\uXXXX. briko.json resolves cheshire when first used, so
   the namespace loads on the JVM without the dependency; babashka
   carries cheshire, and a native image must require `cheshire.core` and
   `cheshire.factory` where it is built.

   `serve-stdio` answers over stdin and stdout and holds at most
   :max-message-bytes characters of a line in memory. The HTTP binding
   (authentication, origin checks, body limits, sessions) stays with the
   caller, because com.sun.net.httpserver is not in babashka; it reads
   with `handle-line` and writes with `write-response`."
  (:require [briko.json :as json]
            [clojure.string :as str])
  (:import (clojure.lang ExceptionInfo)
           (java.io Reader)
           (java.nio.charset StandardCharsets)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def legacy-versions
  "The revisions of the initialize handshake, newest first. initialize
   answers in the client's revision when it is one of them, else in the
   newest."
  ["2025-06-18" "2025-03-26" "2024-11-05"])

(def modern-versions
  "The revisions without a handshake: every request names its revision
   and the client's capabilities in its own `_meta`."
  ["2026-07-28"])

(def meta-protocol-version "io.modelcontextprotocol/protocolVersion")
(def meta-client-capabilities "io.modelcontextprotocol/clientCapabilities")
(def meta-client-info "io.modelcontextprotocol/clientInfo")
(def meta-server-info "io.modelcontextprotocol/serverInfo")

(def parse-error -32700)
(def invalid-request -32600)
(def method-not-found -32601)
(def invalid-params -32602)
(def internal-error -32603)
(def header-mismatch
  "A modern Streamable HTTP request whose headers differ from its body."
  -32020)
(def unsupported-version
  "A modern request naming a revision the server does not speak."
  -32022)

;; Errors

(defn rpc-error!
  "Answers the request being handled with the JSON-RPC error `code`.
   `opts`: :data, the error's data; :status, the HTTP status a modern
   request answers with (200 by default). A legacy request always
   answers 200."
  ([code message] (rpc-error! code message nil))
  ([code message {:keys [data status]}]
   (throw (ex-info message {::code code ::data data ::status (or status 200)}))))

(defn rpc-error?
  "Whether e was thrown by `rpc-error!`."
  [e]
  (some? (::code (ex-data e))))

(defn- error-object
  [id code message data]
  {:jsonrpc "2.0" :id id
   :error (cond-> {:code code :message message} (some? data) (assoc :data data))})

(defn- result-object [id r] {:jsonrpc "2.0" :id id :result r})

;; JSON

(def defaults
  "The options `handle-line`, `read-json` and `serve-stdio` use where the
   caller gives none.

   - :max-message-bytes, the longest message in UTF-8 bytes: 16 MiB."
  {:max-message-bytes 16777216})

(defn- options
  "`opts` merged over `defaults`, refusing a key outside `allowed` and a
   :max-message-bytes that is no positive int below Integer/MAX_VALUE."
  [opts allowed]
  (when-not (or (nil? opts) (map? opts))
    (throw (ex-info "the options must be a map" {:type ::invalid-arguments})))
  (when-let [unknown (seq (remove allowed (keys opts)))]
    (throw (ex-info (str "unknown options " (pr-str (vec unknown)))
                    {:type ::invalid-arguments :unknown (vec unknown)})))
  (let [opts (merge defaults opts)
        limit (:max-message-bytes opts)]
    (when-not (and (int? limit) (< 0 (long limit) Integer/MAX_VALUE))
      (throw (ex-info ":max-message-bytes must be a positive int below Integer/MAX_VALUE"
                      {:type ::invalid-arguments :option :max-message-bytes})))
    opts))

(defn- read-message
  "The JSON value `s` holds, or the briko.json refusal of it."
  [s max-message-bytes]
  (try
    (json/parse-string s {:max-bytes max-message-bytes})
    (catch ExceptionInfo e
      (if (json/refusal? e) e (throw e)))))

(defn- parse-error-answer
  "The -32700 answer to a message briko.json refused. The message and
   :data name the rule (`rule`, and :line and :column where the refusal
   has them); neither quotes the text."
  [message rule position]
  {:response (error-object nil parse-error (str "parse error: " message)
                           (merge {:rule rule} (select-keys position [:line :column])))
   :status 200})

(defn read-json
  "The single JSON value `s` holds, or ::unreadable when briko.json
   refuses it: not exactly one value, longer than :max-message-bytes,
   or breaking one of its rules (see the namespace doc). `opts`:
   :max-message-bytes."
  ([s] (read-json s nil))
  ([s opts]
   (let [msg (read-message s (:max-message-bytes (options opts #{:max-message-bytes})))]
     (if (instance? Throwable msg) ::unreadable msg))))

(defn write-json
  "x as a JSON string, written by briko.json/write-string: hidden code
   points escaped, and a map whose keys print to one name refused with
   that namespace's ex-info."
  [x]
  (json/write-string x))

(defn write-response
  "A response as handle-message returns it, as one line of JSON. A
   response briko.json refuses to write (keys that print to one name, a
   value nested deeper than it prints, a value JSON cannot encode)
   becomes a -32603 error for the same id, naming the rule."
  [response]
  (try
    (json/write-string response)
    (catch ExceptionInfo e
      (if (json/refusal? e)
        (json/write-string
         (error-object (:id response) internal-error
                       (str "internal error: the response cannot be written as JSON: " (ex-message e))
                       {:rule (name (:type (ex-data e)))}))
        (throw e)))))

;; Headers

(defn- one-header
  "The value of header h (lower-case): a string as it is, a collection
   only when it holds exactly one value. A header sent twice is treated
   as missing."
  [headers h]
  (let [v (get headers h)]
    (cond
      (string? v) v
      (and (coll? v) (= 1 (count v)) (string? (first v))) (first v)
      :else nil)))

(defn header-value
  "A mirrored header value as the client meant it: the Base64 sentinel
   form `=?base64?…?=` decoded, anything else as sent. An undecodable
   sentinel is ::undecodable, which equals no parameter."
  [v]
  (if-let [[_ b64] (and (string? v) (re-matches #"=\?base64\?([A-Za-z0-9+/]*={0,2})\?=" v))]
    (try (String. (.decode (Base64/getDecoder) ^String b64) StandardCharsets/UTF_8)
         (catch IllegalArgumentException _ ::undecodable))
    v))

(def ^:private named-param
  {"tools/call" "name" "resources/read" "uri" "prompts/get" "name"})

(defn- check-headers!
  "The Streamable HTTP rule of the modern revisions: the version, method
   and name the body carries must stand in the headers, unchanged, so
   nothing that routes on the headers sees another request than the one
   that runs."
  [headers method params version]
  (let [mismatch #(rpc-error! header-mismatch (str "header mismatch: " %) {:status 400})]
    (when-not (= version (one-header headers "mcp-protocol-version"))
      (mismatch "MCP-Protocol-Version must equal the request's protocol version"))
    (when-not (= method (one-header headers "mcp-method"))
      (mismatch "Mcp-Method must equal the request's method"))
    (when-let [field (named-param method)]
      (when-not (= (get params field) (header-value (one-header headers "mcp-name")))
        (mismatch (str "Mcp-Name must equal params." field))))))

;; Eras

(defn modern?
  "Whether a parsed message is a request of a modern revision. `headers`
   are the HTTP request's (lower-case names), nil on stdio."
  [{:strs [method params]} headers]
  (and (not= "initialize" method)
       (or (= "server/discover" method)
           (and (map? params) (some? (get-in params ["_meta" meta-protocol-version])))
           (boolean (some #{(one-header headers "mcp-protocol-version")} modern-versions)))))

(defn- answer
  "The result of a method both eras answer, or ::unknown."
  [{:keys [handlers]} method params request]
  (if-let [f (get handlers method)]
    (f params request)
    (if (= "ping" method) {} ::unknown)))

(defn- modern-result
  [server r]
  (-> r
      (update :resultType #(or % "complete"))
      (assoc-in [:_meta meta-server-info] (:server-info server))))

(defn- check-modern!
  [method params headers]
  (let [version (get-in params ["_meta" meta-protocol-version])]
    (when-not (string? version)
      (rpc-error! invalid-params (str "the request's _meta needs " meta-protocol-version) {:status 400}))
    (when-not (some #{version} modern-versions)
      (rpc-error! unsupported-version "Unsupported protocol version"
                  {:data {:supported modern-versions :requested version} :status 400}))
    (when-not (map? (get-in params ["_meta" meta-client-capabilities]))
      (rpc-error! invalid-params (str "the request's _meta needs " meta-client-capabilities) {:status 400}))
    (when headers (check-headers! headers method params version))))

(defn- modern
  [server id method params request]
  (if (= "server/discover" method)
    (result-object id (modern-result server
                                     (cond-> (merge (:discover server)
                                                    {:supportedVersions modern-versions
                                                     :capabilities (:capabilities server)})
                                       (:instructions server) (assoc :instructions (:instructions server)))))
    (let [r (answer server method params request)]
      (when (= ::unknown r)
        (rpc-error! method-not-found (str "method not found: " method) {:status 404}))
      (result-object id (modern-result server r)))))

(defn- legacy
  [server id method params request]
  (if (= "initialize" method)
    (result-object id (cond-> {:protocolVersion (let [asked (get params "protocolVersion")]
                                                  (if (some #{asked} legacy-versions) asked (first legacy-versions)))
                               :capabilities (:capabilities server)
                               :serverInfo (:server-info server)}
                        (:instructions server) (assoc :instructions (:instructions server))))
    (let [r (answer server method params request)]
      (when (= ::unknown r)
        (rpc-error! method-not-found (str "method not found: " method)))
      (result-object id r))))

(defn- valid-id? [id] (or (string? id) (integer? id)))

(defn handle-message
  "`{:response r :status s}` for one parsed JSON-RPC message `msg`
   (string keys, as briko.json reads JSON): r is the response map, nil for
   a notification, and s the HTTP status it calls for. `opts`:
   :headers, the HTTP request's headers as lower-case names to a string
   or a collection of strings; nil or absent on stdio."
  ([server msg] (handle-message server msg nil))
  ([server msg {:keys [headers]}]
   (if-not (map? msg)
     {:response (error-object nil invalid-request "invalid request: not a JSON object" nil) :status 200}
     (let [{:strs [id method params]} msg
           notification? (and (not (contains? msg "id")) (string? method) (= "2.0" (get msg "jsonrpc")))
           era (if (modern? msg headers) :modern :legacy)
           reply-id (when (valid-id? id) id)]
       (try
         (cond
           (not= "2.0" (get msg "jsonrpc"))
           (rpc-error! invalid-request "invalid request: jsonrpc must be \"2.0\"" {:status 400})

           (not (string? method))
           (rpc-error! invalid-request "invalid request: method must be a string" {:status 400})

           (and (not notification?) (not (valid-id? id)))
           (rpc-error! invalid-request "invalid request: id must be a string or an integer" {:status 400})

           (and (contains? msg "params") (not (map? params)))
           (rpc-error! invalid-params "params must be an object" {:status 400})

           :else
           (let [params (or params {})
                 request {:era era :id id :method method :headers headers}]
             (when (= :modern era) (check-modern! method params headers))
             (cond
               notification? {:response nil :status 202}
               (= :modern era) {:response (modern server id method params request) :status 200}
               :else {:response (legacy server id method params request) :status 200})))
         ;; The protocol boundary: any failure, an Error included, becomes
         ;; a JSON-RPC error answer instead of ending the session.
         #_{:splint/disable [lint/catch-throwable]}
         (catch Throwable e
           (let [{::keys [code data status]} (ex-data e)
                 status (if (and code (= :modern era)) status 200)]
             (cond
               notification? {:response nil :status (if (= 200 status) 202 status)}
               code {:response (error-object reply-id code (ex-message e) data) :status status}
               :else {:response (error-object reply-id internal-error
                                              (str "internal error: " (or (ex-message e) (.getName (class e))))
                                              nil)
                      :status 200}))))))))

(defn handle-line
  "`{:response r :status s}` for one JSON-RPC message as text, as
   handle-message answers it. Text that briko.json refuses (see the
   namespace doc) answers -32700 with a null id. `opts`: :headers, as
   handle-message takes them; :max-message-bytes."
  ([server line] (handle-line server line nil))
  ([server line opts]
   (let [{:keys [max-message-bytes headers]} (options opts #{:headers :max-message-bytes})
         msg (read-message line max-message-bytes)]
     (if (instance? Throwable msg)
       (let [data (ex-data msg)]
         (parse-error-answer (ex-message msg) (name (:type data)) data))
       (handle-message server msg {:headers headers})))))

;; Tools

(defn tool-handlers
  "The `tools/list` and `tools/call` handlers over `tools`, a sequence of
   maps with :name, :description, :inputSchema and :call. :call is
   `(fn [arguments request] {:text s :error? b})`: the tool's answer as
   text, and whether it is a tool error, which the client sees as
   `isError` in the result. tools/list lists every key but :call.

   A call naming no known tool, or carrying arguments that are not an
   object, answers -32602; absent arguments are an empty object."
  [tools]
  (let [by-name (zipmap (map :name tools) tools)
        listed (mapv #(dissoc % :call) tools)]
    {"tools/list" (fn [_ _] {:tools listed})
     "tools/call" (fn [params request]
                    (let [tool-name (get params "name")
                          args (get params "arguments" {})
                          tool (when (string? tool-name) (get by-name tool-name))]
                      (cond
                        (not (string? tool-name)) (rpc-error! invalid-params "tools/call needs a tool name")
                        (nil? tool) (rpc-error! invalid-params (str "unknown tool " tool-name))
                        (not (map? args)) (rpc-error! invalid-params "tool arguments must be an object")
                        :else (let [{:keys [text error?]} ((:call tool) args request)]
                                {:content [{:type "text" :text (str text)}]
                                 :isError (boolean error?)}))))}))

(defn server
  "A server map from `opts` (see the namespace doc), checking the
   required keys."
  [{:keys [server-info capabilities handlers] :as opts}]
  (when-not (and (map? server-info) (string? (:name server-info)) (not (str/blank? (:name server-info))))
    (throw (ex-info "a server needs :server-info with a :name" {:type ::invalid-arguments})))
  (when-not (map? capabilities)
    (throw (ex-info "a server needs a :capabilities map" {:type ::invalid-arguments})))
  (when-not (and (map? handlers) (every? string? (keys handlers)) (every? fn? (vals handlers)))
    (throw (ex-info "a server needs :handlers, method names to functions" {:type ::invalid-arguments})))
  opts)

(defn- line-reader
  "A function returning the next line of `r` on each call: the line
   without its newline (and a carriage return before it), ::too-long for
   a line of more than `limit` characters, or nil once `r` ends. It holds
   at most `limit` characters of a line plus one read buffer; the rest of
   a line that is too long is read and dropped. A line of more than
   `limit` characters has more than `limit` UTF-8 bytes, since no
   character takes fewer bytes than UTF-16 units."
  [^Reader r ^long limit]
  (let [buf (char-array 8192)
        pending (StringBuilder.)
        state (volatile! {:scanned 0 :skipping? false :ended? false})
        take! (fn [^long end]
                ;; what pending holds up to end, dropping one more
                ;; character (the newline), and giving back the capacity
                ;; a long line left behind
                (let [text (.substring pending 0 end)]
                  (.delete pending 0 (min (.length pending) (inc end)))
                  (when (> (.capacity pending) 65536) (.trimToSize pending))
                  (vswap! state assoc :scanned 0 :skipping? false)
                  text))]
    (fn next-line []
      (let [{:keys [scanned skipping? ended?]} @state
            nl (.indexOf pending "\n" (int scanned))]
        (cond
          (>= nl 0)
          (let [text (take! nl)
                text (if (str/ends-with? text "\r") (subs text 0 (dec (count text))) text)]
            (if (or skipping? (> (count text) limit)) ::too-long text))

          ended?
          (let [text (take! (.length pending))]
            (cond
              skipping? ::too-long
              (zero? (count text)) nil
              (> (count text) limit) ::too-long
              :else text))

          :else
          (do
            ;; one more than limit leaves room for the carriage return
            (when (or skipping? (> (.length pending) (inc limit)))
              (.setLength pending 0)
              (when-not skipping? (.trimToSize pending))
              (vswap! state assoc :skipping? true))
            (let [n (.read r buf)]
              (vswap! state assoc :scanned (.length pending))
              (if (neg? n)
                (vswap! state assoc :ended? true)
                (.append pending buf 0 n)))
            (recur)))))))

(defn serve-stdio
  "Answers one JSON-RPC message per line of *in* on *out*, one response
   per line, flushed after each, until *in* ends. A notification writes
   nothing. A line longer than :max-message-bytes answers -32700 without
   being held in memory whole, and the next line is read as usual.
   `opts`: :max-message-bytes."
  ([server] (serve-stdio server nil))
  ([server opts]
   (let [{:keys [max-message-bytes]} (options opts #{:max-message-bytes})
         next-line (line-reader *in* max-message-bytes)
         too-long (parse-error-answer (format "the input is larger than %d bytes" max-message-bytes)
                                      (name ::json/too-large) nil)]
     (loop []
       (when-let [line (next-line)]
         (let [answer (if (= ::too-long line)
                        too-long
                        (handle-line server line {:max-message-bytes max-message-bytes}))]
           (when-let [response (:response answer)]
             (println (write-response response))
             (flush)))
         (recur))))))
