;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.mcp-test
  "Message handling against the request and response pairs tik's and
   kajero's MCP tests record, plus the cases where briko answers
   differently from one of them."
  (:require [briko.mcp :as mcp]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.util Base64)))

(def ^:private calls (atom []))

(def ^:private tools
  [{:name "echo"
    :description "Echoes its argument."
    :inputSchema {:type "object" :properties {:text {:type "string"}} :required ["text"]}
    :call (fn [args request]
            (swap! calls conj [args (:era request)])
            (if (string? (get args "text"))
              {:text (get args "text")}
              {:text "missing required argument(s): text" :error? true}))}
   {:name "boom"
    :description "Fails."
    :inputSchema {:type "object" :properties {}}
    :call (fn [_ _] (throw (StackOverflowError.)))}])

(def ^:private skills-extension "io.modelcontextprotocol/skills")

(def ^:private server
  (mcp/server
   {:server-info {:name "demo" :version "1"}
    :capabilities {:tools {} :resources {} :extensions {skills-extension {}}}
    :instructions "Read skill://demo/SKILL.md first."
    :discover {:ttlMs 300000 :cacheScope "public"}
    :handlers (merge (mcp/tool-handlers tools)
                     {"resources/templates/list" (fn [_ _] {:resourceTemplates []})
                      "resources/read" (fn [params _]
                                         (let [uri (get params "uri")]
                                           (if (= "skill://demo/SKILL.md" uri)
                                             {:contents [{:uri uri :text "# Demo"}]}
                                             (mcp/rpc-error! mcp/invalid-params (str "no resource at " uri)))))})}))

(def ^:private bare-server
  (mcp/server {:server-info {:name "bare" :version "1"}
               :capabilities {:tools {}}
               :handlers (mcp/tool-handlers [])}))

(def ^:private modern-meta
  {"io.modelcontextprotocol/protocolVersion" "2026-07-28"
   "io.modelcontextprotocol/clientInfo" {"name" "test" "version" "1"}
   "io.modelcontextprotocol/clientCapabilities" {}})

(defn- wire
  "The response as a client reads it: through JSON and back."
  [r]
  (some-> r json/generate-string json/parse-string))

(defn- line
  ([text] (line server text nil))
  ([srv text headers]
   (update (mcp/handle-line srv text {:headers headers}) :response wire)))

(defn- rpc
  ([method params] (rpc method params 1))
  ([method params id]
   (:response (line (json/generate-string (cond-> {"jsonrpc" "2.0" "id" id "method" method}
                                            params (assoc "params" params)))))))

(defn- modern [method params]
  (rpc method (assoc params "_meta" modern-meta)))

(defn- mirrored
  "The headers a modern client sends for method and params."
  [method params]
  (cond-> {"mcp-protocol-version" "2026-07-28" "mcp-method" method}
    (= "tools/call" method) (assoc "mcp-name" (get params "name"))
    (= "resources/read" method) (assoc "mcp-name" (get params "uri"))))

(defn- http
  "A modern request over HTTP: the body carries the _meta, headers as
   given (nil values removed)."
  ([method params headers] (http server method params headers))
  ([srv method params headers]
   (line srv (json/generate-string {"jsonrpc" "2.0" "id" 1 "method" method
                                    "params" (assoc params "_meta" modern-meta)})
         (into {} (remove (comp nil? val)) headers))))

;; Recorded pairs: tik test/tik/mcp_http_test.clj and mcp_skills_test.clj,
;; kajero test/kajero/mcp_test.clj (origin/main 4cf5f6a and 866cde7).

(deftest initialize-answers-in-the-clients-revision-else-the-newest
  (doseq [[asked answered] [["2025-06-18" "2025-06-18"] ["2025-03-26" "2025-03-26"] ["2024-11-05" "2024-11-05"]
                            ["1999-01-01" "2025-06-18"] ["2026-07-28" "2025-06-18"] [nil "2025-06-18"]]]
    (is (= answered (get-in (rpc "initialize" (when asked {"protocolVersion" asked})) ["result" "protocolVersion"]))
        (pr-str asked)))
  (let [r (get (rpc "initialize" {"protocolVersion" "2025-06-18"}) "result")]
    (is (= {"name" "demo" "version" "1"} (get r "serverInfo")))
    (is (= {} (get-in r ["capabilities" "extensions" skills-extension])))
    (is (= {} (get-in r ["capabilities" "tools"])))
    (is (str/includes? (get r "instructions") "skill://demo/SKILL.md")))
  (testing "a server without instructions announces none"
    (let [r (get (:response (line bare-server "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}" nil)) "result")]
      (is (= {"tools" {}} (get r "capabilities")))
      (is (not (contains? r "instructions"))))))

(deftest a-notification-gets-no-response
  (is (= {:response nil :status 202} (line "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))
  (testing "a notification naming a request method runs nothing"
    (reset! calls [])
    (is (nil? (:response (line "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"text\":\"x\"}}}"))))
    (is (= [] @calls)))
  (testing "a modern notification is checked like a request"
    (is (= {:response nil :status 202}
           (line server (json/generate-string {"jsonrpc" "2.0" "method" "notifications/cancelled" "params" {"_meta" modern-meta}})
                 (mirrored "notifications/cancelled" {}))))
    (is (= {:response nil :status 400}
           (line server (json/generate-string {"jsonrpc" "2.0" "method" "notifications/cancelled" "params" {"_meta" modern-meta}})
                 {"mcp-protocol-version" "2026-07-28" "mcp-method" "other"}))))
  (testing "negative control: the same message with an id is answered"
    (is (some? (:response (line "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"notifications/initialized\"}"))))))

(deftest tools-are-listed-and-called
  (is (= ["echo" "boom"] (map #(get % "name") (get-in (rpc "tools/list" {}) ["result" "tools"]))))
  (is (not-any? #(contains? % "call") (get-in (rpc "tools/list" {}) ["result" "tools"])))
  (is (= {"content" [{"type" "text" "text" "hi"}] "isError" false}
         (get (rpc "tools/call" {"name" "echo" "arguments" {"text" "hi"}}) "result")))
  (testing "a tool's own refusal is a tool result, not a protocol error"
    (is (true? (get-in (rpc "tools/call" {"name" "echo" "arguments" {}}) ["result" "isError"])))
    (is (true? (get-in (rpc "tools/call" {"name" "echo"}) ["result" "isError"])) "absent arguments are {}"))
  (testing "an unknown tool, a missing name and arguments that are not an object are invalid params"
    (doseq [params [{"name" "no_such_tool"} {} {"name" 7} {"name" "echo" "arguments" "not-a-map"}
                    {"name" "echo" "arguments" nil} {"name" "echo" "arguments" [1]}]]
      (is (= -32602 (get-in (rpc "tools/call" params) ["error" "code"])) (pr-str params))))
  (testing "a tool that throws an Error answers -32603 and the session goes on"
    (let [e (get (rpc "tools/call" {"name" "boom"}) "error")]
      (is (= -32603 (get e "code")))
      (is (str/includes? (get e "message") "StackOverflowError")))
    (is (= "hi" (get-in (rpc "tools/call" {"name" "echo" "arguments" {"text" "hi"}} 99) ["result" "content" 0 "text"]))))
  (testing "the handler learns the era"
    (reset! calls [])
    (modern "tools/call" {"name" "echo" "arguments" {"text" "m"}})
    (rpc "tools/call" {"name" "echo" "arguments" {"text" "l"}})
    (is (= [[{"text" "m"} :modern] [{"text" "l"} :legacy]] @calls))))

(deftest unreadable-input-answers-a-parse-error-and-the-session-goes-on
  (doseq [text ["not json" "not json at all {{{" "" "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"} trailing"
                "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}" nil]]
    (let [{:keys [response status]} (line server text nil)]
      (is (= 200 status) (pr-str text))
      (is (= {"jsonrpc" "2.0" "id" nil "error" {"code" -32700 "message" "parse error: not a JSON value"}} response)
          (pr-str text))))
  (testing "negative control: one value with trailing whitespace is read"
    (is (= {} (get-in (line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}  \n") [:response "result"])))))

(deftest a-value-that-is-no-request-is-an-invalid-request
  (doseq [text ["[1,2,3]" "5" "null" "\"a bare string\"" "{\"id\":1,\"method\":\"ping\"}"
                "{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"ping\"}" "{\"jsonrpc\":\"2.0\",\"id\":1}"
                "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":7}" "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"ping\"}"
                "{\"jsonrpc\":\"2.0\",\"id\":1.5,\"method\":\"ping\"}" "{\"jsonrpc\":\"2.0\",\"id\":{},\"method\":\"ping\"}"]]
    (let [{:keys [response status]} (line text)]
      (is (= -32600 (get-in response ["error" "code"])) text)
      (is (= 200 status) text)))
  (testing "the id of an invalid request is echoed when it is one"
    (is (= 7 (get (:response (line "{\"jsonrpc\":\"1.0\",\"id\":7,\"method\":\"ping\"}")) "id")))
    (is (nil? (get (:response (line "{\"jsonrpc\":\"2.0\",\"id\":1.5,\"method\":\"ping\"}")) "id"))))
  (testing "params that are not an object are invalid params"
    (is (= -32602 (get-in (:response (line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":[1]}")) ["error" "code"]))))
  (testing "string ids are answered as sent"
    (is (= "abc" (get (rpc "ping" nil "abc") "id")))))

(deftest an-unknown-method-is-not-found-in-both-eras
  (is (= -32601 (get-in (rpc "bogus" {}) ["error" "code"])))
  (is (= -32601 (get-in (rpc "skills/list" {}) ["error" "code"])) "a method no handler serves")
  (is (= 200 (:status (line server "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"bogus\"}" {"mcp-protocol-version" "2025-06-18"})))
      "a legacy request over HTTP answers 200")
  (is (= -32601 (get-in (modern "nope/nothing" {}) ["error" "code"])))
  (is (= 404 (:status (http "prompts/list" {} (mirrored "prompts/list" {})))))
  (testing "negative control: ping is answered without a handler"
    (is (= {} (get (rpc "ping" nil) "result")))
    (is (= "complete" (get-in (modern "ping" {}) ["result" "resultType"])))))

(deftest server-discover-names-versions-capabilities-and-instructions
  (let [r (get (modern "server/discover" {}) "result")]
    (is (= "complete" (get r "resultType")))
    (is (= ["2026-07-28"] (get r "supportedVersions")))
    (is (= 300000 (get r "ttlMs")))
    (is (= "public" (get r "cacheScope")))
    (is (= {} (get-in r ["capabilities" "extensions" skills-extension])))
    (is (= {"name" "demo" "version" "1"} (get-in r ["_meta" "io.modelcontextprotocol/serverInfo"])))
    (is (str/includes? (get r "instructions") "skill://demo/SKILL.md")))
  (testing "server/discover without _meta is a malformed modern request"
    (let [{:keys [response status]} (line server "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"server/discover\",\"params\":{}}"
                                          {"mcp-method" "server/discover"})]
      (is (= 400 status))
      (is (= -32602 (get-in response ["error" "code"]))))))

(deftest a-modern-request-names-its-version-and-capabilities
  (let [e (get (rpc "tools/list" {"_meta" (assoc modern-meta "io.modelcontextprotocol/protocolVersion" "1900-01-01")}) "error")]
    (is (= -32022 (get e "code")))
    (is (= "Unsupported protocol version" (get e "message")))
    (is (= {"supported" ["2026-07-28"] "requested" "1900-01-01"} (get e "data"))))
  (is (= -32602 (get-in (rpc "tools/list" {"_meta" (dissoc modern-meta "io.modelcontextprotocol/clientCapabilities")})
                        ["error" "code"])))
  (is (= -32602 (get-in (rpc "tools/list" {"_meta" (assoc modern-meta "io.modelcontextprotocol/protocolVersion" 7)})
                        ["error" "code"])))
  (let [r (get (modern "tools/list" {}) "result")]
    (is (= "complete" (get r "resultType")))
    (is (= {"name" "demo" "version" "1"} (get-in r ["_meta" "io.modelcontextprotocol/serverInfo"])))
    (is (= 2 (count (get r "tools")))))
  (testing "a result that names its own resultType keeps it"
    (let [srv (mcp/server {:server-info {:name "x"} :capabilities {}
                           :handlers {"skills/list" (fn [_ _] {:resultType "partial" :skills []})}})]
      (is (= "partial" (get-in (:response (line srv (json/generate-string {"jsonrpc" "2.0" "id" 1 "method" "skills/list"
                                                                           "params" {"_meta" modern-meta}}) nil))
                               ["result" "resultType"])))))
  (testing "legacy results carry no modern decoration"
    (is (not (contains? (get (rpc "tools/list" {}) "result") "resultType")))))

(deftest modern-http-requests-mirror-their-body-into-headers
  (is (= 200 (:status (http "tools/list" {} (mirrored "tools/list" {})))))
  (is (= 200 (:status (http "resources/read" {"uri" "skill://demo/SKILL.md"} (mirrored "resources/read" {"uri" "skill://demo/SKILL.md"})))))
  (testing "headers given as one-element collections, as com.sun.net.httpserver hands them over"
    (is (= 200 (:status (http "tools/list" {} (update-vals (mirrored "tools/list" {}) vector))))))
  (testing "a Base64-sentinel Mcp-Name is decoded before it is compared"
    (let [b64 (str "=?base64?" (.encodeToString (Base64/getEncoder) (.getBytes "skill://demo/SKILL.md" "UTF-8")) "?=")]
      (is (= 200 (:status (http "resources/read" {"uri" "skill://demo/SKILL.md"}
                                (assoc (mirrored "resources/read" {"uri" "skill://demo/SKILL.md"}) "mcp-name" b64)))))))
  (let [read-params {"uri" "skill://demo/SKILL.md"}
        read-headers (mirrored "resources/read" read-params)
        call-params {"name" "echo" "arguments" {"text" "x"}}
        call-headers (mirrored "tools/call" call-params)]
    (doseq [[why method params headers]
            [["version mismatch" "tools/list" {} (assoc (mirrored "tools/list" {}) "mcp-protocol-version" "2025-06-18")]
             ["version missing" "tools/list" {} (dissoc (mirrored "tools/list" {}) "mcp-protocol-version")]
             ["method mismatch" "resources/read" read-params (assoc read-headers "mcp-method" "tools/list")]
             ["method missing" "resources/read" read-params (dissoc read-headers "mcp-method")]
             ["name mismatch" "resources/read" read-params (assoc read-headers "mcp-name" "skill://demo/other.md")]
             ["name missing" "resources/read" read-params (dissoc read-headers "mcp-name")]
             ["tool name mismatch" "tools/call" call-params (assoc call-headers "mcp-name" "boom")]
             ["a name sent twice" "resources/read" read-params (assoc read-headers "mcp-name" ["skill://demo/SKILL.md" "skill://demo/SKILL.md"])]
             ["a version sent twice" "tools/list" {} (assoc (mirrored "tools/list" {}) "mcp-protocol-version" ["2026-07-28" "2026-07-28"])]
             ["a bad base64 name" "resources/read" read-params (assoc read-headers "mcp-name" "=?base64?!!!?=")]
             ["base64 padding inside" "resources/read" read-params (assoc read-headers "mcp-name" "=?base64?c2=tpbGw=?=")]]]
      (let [{:keys [status response]} (http method params headers)]
        (is (= 400 status) why)
        (is (= -32020 (get-in response ["error" "code"])) why))))
  (testing "an unsupported revision over HTTP is 400"
    (let [{:keys [status response]} (line server (json/generate-string {"jsonrpc" "2.0" "id" 1 "method" "tools/list"
                                                                        "params" {"_meta" (assoc modern-meta "io.modelcontextprotocol/protocolVersion" "1900-01-01")}})
                                          {"mcp-protocol-version" "1900-01-01" "mcp-method" "tools/list"})]
      (is (= 400 status))
      (is (= -32022 (get-in response ["error" "code"])))))
  (testing "a modern header without _meta in the body is a malformed modern request"
    (doseq [[why params] [["no clientCapabilities" {"_meta" (dissoc modern-meta "io.modelcontextprotocol/clientCapabilities")}]
                          ["no protocolVersion" {"_meta" (dissoc modern-meta "io.modelcontextprotocol/protocolVersion")}]
                          ["no _meta at all" {}]]]
      (let [{:keys [status response]} (line server (json/generate-string {"jsonrpc" "2.0" "id" 1 "method" "tools/list" "params" params})
                                            (mirrored "tools/list" {}))]
        (is (= 400 status) why)
        (is (= -32602 (get-in response ["error" "code"])) why))))
  (testing "a handler's refusal is an error in the body, not an HTTP failure"
    (let [{:keys [status response]} (http "resources/read" {"uri" "skill://demo/x.md"} (mirrored "resources/read" {"uri" "skill://demo/x.md"}))]
      (is (= 200 status))
      (is (= -32602 (get-in response ["error" "code"])))))
  (testing "a legacy client on the same endpoint is unaffected by missing headers"
    (let [{:keys [status response]} (line server "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\"}}" {})]
      (is (= 200 status))
      (is (= "demo" (get-in response ["result" "serverInfo" "name"])))))
  (testing "stdio checks no headers"
    (is (= "complete" (get-in (modern "resources/read" {"uri" "skill://demo/SKILL.md"}) ["result" "resultType"])))))

(deftest modern-is-told-from-the-message-and-the-headers
  (is (true? (mcp/modern? {"method" "tools/list" "params" {"_meta" modern-meta}} nil)))
  (is (true? (mcp/modern? {"method" "server/discover"} nil)))
  (is (true? (mcp/modern? {"method" "tools/list"} {"mcp-protocol-version" "2026-07-28"})))
  (is (true? (mcp/modern? {"method" "tools/list"} {"mcp-protocol-version" ["2026-07-28"]})))
  (is (false? (mcp/modern? {"method" "initialize" "params" {"_meta" modern-meta}} {"mcp-protocol-version" "2026-07-28"})))
  (is (false? (mcp/modern? {"method" "tools/list"} {"mcp-protocol-version" "2025-06-18"})))
  (is (false? (mcp/modern? {"method" "tools/list"} nil))))

(deftest header-values-decode-only-the-sentinel-form
  (is (= "skill://x/SKILL.md" (mcp/header-value "skill://x/SKILL.md")))
  (is (= "é" (mcp/header-value (str "=?base64?" (.encodeToString (Base64/getEncoder) (.getBytes "é" "UTF-8")) "?="))))
  (is (= ::mcp/undecodable (mcp/header-value "=?base64?A?=")))
  (is (= "=?base64?a b?=" (mcp/header-value "=?base64?a b?=")) "not the sentinel form, so taken as sent")
  (is (nil? (mcp/header-value nil))))

(deftest a-server-map-is-checked
  (doseq [bad [{} {:server-info {:name ""} :capabilities {} :handlers {}}
               {:server-info {:name "x"} :handlers {}}
               {:server-info {:name "x"} :capabilities {} :handlers {:tools/list identity}}
               {:server-info {:name "x"} :capabilities {} :handlers {"a" "not a fn"}}]]
    (is (thrown? clojure.lang.ExceptionInfo (mcp/server bad)) (pr-str bad)))
  (is (map? (mcp/server {:server-info {:name "x"} :capabilities {} :handlers {}}))))

(deftest rpc-errors-carry-code-data-and-status
  (let [e (try (mcp/rpc-error! -32602 "no" {:data {:a 1} :status 400}) (catch clojure.lang.ExceptionInfo e e))]
    (is (mcp/rpc-error? e))
    (is (= "no" (ex-message e))))
  (is (not (mcp/rpc-error? (ex-info "other" {})))))

(deftest read-json-takes-exactly-one-value
  (is (= {"a" 1} (mcp/read-json "{\"a\":1}")))
  (is (nil? (mcp/read-json "null")))
  (is (= ::mcp/unreadable (mcp/read-json "{\"a\":1} 2")))
  (is (= ::mcp/unreadable (mcp/read-json "")))
  (is (= ::mcp/unreadable (mcp/read-json 42)))
  (is (= "{\"a\":1}" (mcp/write-json {:a 1}))))

(deftest stdio-answers-one-line-per-request
  (let [in (str "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}\n"
                "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                "not json\n"
                "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"tools/list\"}\n")
        out (with-out-str (with-in-str in (mcp/serve-stdio server)))
        lines (mapv json/parse-string (str/split-lines out))]
    (is (= [1 nil 99] (mapv #(get % "id") lines)) "the notification wrote nothing")
    (is (= -32700 (get-in lines [1 "error" "code"])))
    (is (= "echo" (get-in lines [2 "result" "tools" 0 "name"])))))
