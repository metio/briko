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

   JSON is read and written with cheshire, resolved when first used, so
   the namespace loads on the JVM without the dependency; babashka
   carries cheshire, and a native image must require `cheshire.core`
   where it is built. `serve-stdio` answers over stdin and stdout; the
   HTTP binding (authentication, origin checks, body limits, sessions)
   stays with the caller, because com.sun.net.httpserver is not in
   babashka."
  (:require [clojure.string :as str])
  (:import (java.io StringReader)
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

(defn- json-fn [sym] (requiring-resolve sym))

(defn read-json
  "The single JSON value `s` holds, or ::unreadable when it is not
   exactly one well-formed JSON value. Text after the value is refused:
   a reader that stops at the first value would answer a request other
   than the one the bytes hold."
  [s]
  (if-not (string? s)
    ::unreadable
    (try
      (let [values (doall (take 2 ((json-fn 'cheshire.core/parsed-seq) (StringReader. ^String s))))]
        (if (= 1 (count values)) (first values) ::unreadable))
      (catch Exception _ ::unreadable))))

(defn write-json
  "x as a JSON string."
  [x]
  ((json-fn 'cheshire.core/generate-string) x))

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
   (string keys, as cheshire reads JSON): r is the response map, nil for
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
   handle-message answers it. Text that is not exactly one JSON value
   answers -32700 with a null id."
  ([server line] (handle-line server line nil))
  ([server line opts]
   (let [msg (read-json line)]
     (if (= ::unreadable msg)
       {:response (error-object nil parse-error "parse error: not a JSON value" nil) :status 200}
       (handle-message server msg opts)))))

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

(defn serve-stdio
  "Answers one JSON-RPC message per line of *in* on *out*, one response
   per line, flushed after each, until *in* ends. A notification writes
   nothing."
  [server]
  (loop []
    (when-let [line (read-line)]
      (when-let [response (:response (handle-line server line))]
        (println (write-json response))
        (flush))
      (recur))))
