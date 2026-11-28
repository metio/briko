;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [briko.json :as json]
            [cheshire.core :as cheshire]
            [cheshire.factory :as factory]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.io ByteArrayInputStream InputStream)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (json/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- refused
  ([s] (refusal #(json/parse-string s)))
  ([s opts] (refusal #(json/parse-string s opts))))

(defn- thrown
  "The exception (f) throws, or nil."
  [f]
  (try (f) nil (catch Exception e e)))

(defn- nest
  [depth inner]
  (str (str/join (repeat depth "[")) inner (str/join (repeat depth "]"))))

(defn- bytes-of
  ^bytes [xs]
  (byte-array (map unchecked-byte xs)))

(defn- with-temp-dir*
  [f]
  (let [dir (fs/create-temp-dir {:prefix "briko-json-"})]
    (try (f dir) (finally (fs/delete-tree dir)))))

;; Reading

(deftest reads-rfc-8259-json
  (is (= {"a" [1 2.5 "x" true false nil] "b" {"c" {}}}
         (json/parse-string "{\"a\":[1,2.5,\"x\",true,false,null],\"b\":{\"c\":{}}}")))
  (is (= 42 (json/parse-string " 42 \n")))
  (is (= "é\u0001" (json/parse-string "\"\\u00e9\\u0001\"")))
  (is (nil? (json/parse-string "null")))
  (testing "integers stay exact, decimals are doubles"
    (is (= 123456789012345678901234567890N (json/parse-string "123456789012345678901234567890")))
    (is (instance? Double (json/parse-string "1.5")))
    (is (= 0.0 (json/parse-string "1e-400")) "an underflow reads as zero"))
  (testing "a top-level array arrives whole, never as a lazy seq"
    (is (vector? (json/parse-string "[1,2]")))))

(deftest every-entry-point-reads-the-same-text
  (with-temp-dir*
    (fn [dir]
      (let [text "{\"a\":[1,{\"b\":\"é\"}]}"
            expected {"a" [1 {"b" "é"}]}
            file (fs/path dir "x.json")
            bs (.getBytes text "UTF-8")]
        (fs/write-bytes file bs)
        (is (= expected (json/parse-string text)))
        (is (= expected (json/parse-bytes bs)))
        (is (= expected (json/parse-stream (ByteArrayInputStream. bs))))
        (is (= expected (json/parse-file file)))
        (is (= expected (json/parse-file (str file))))))))

(deftest a-truncated-document-is-refused-inside-the-call
  (doseq [s ["[{\"time\":\"2026-01-01T00:00:00Z\"}, {\"ti" "[1,2" "{\"a\":[1,{\"b\":2}" "\"open"]]
    (is (= ::json/malformed (refused s)) s)))

(deftest text-after-the-value-is-refused
  (doseq [s ["[1] trailing" "[1] null" "{\"a\":1}{\"b\":2}" "1 2" "[1] ]"]]
    (is (= ::json/trailing-value (refused s)) s))
  (is (= ::json/malformed (refused "repository abc opened (version 2)\n[]")))
  (testing "negative control: whitespace after the value is not text"
    (is (= [1] (json/parse-string "[1] \n\t\r ")))))

(deftest every-value-is-read-when-asked
  (is (= [{"a" 1} [2] 3] (json/parse-string "{\"a\":1}\n[2]\n3\n" {:one-value? false})))
  (is (= [] (json/parse-string " " {:one-value? false})))
  (is (= ::json/malformed (refused "{\"a\":1}\n[2" {:one-value? false})) "a truncated last value")
  (is (= ::json/duplicate-key (refused "{}\n{\"a\":1,\"a\":2}" {:one-value? false})))
  (is (= ::json/unexpected (refused "{}\n[]" {:one-value? false :expect map?})) "every value meets :expect")
  (is (= [{:a 1}] (json/parse-string "{\"a\":1}" {:one-value? false :keys :keyword}))))

(deftest no-value-is-refused
  (is (= ::json/no-value (refused "")))
  (is (= ::json/no-value (refused " \n ")))
  (is (= ::json/invalid-input (refused nil)))
  (is (= ::json/invalid-input (refusal #(json/parse-bytes "x"))))
  (is (= ::json/invalid-input (refusal #(json/parse-stream "x")))))

(deftest anything-beyond-rfc-8259-is-refused
  (doseq [s ["[1 /* c */]" "// c\n[1]" "['a']" "{a:1}" "[NaN]" "[Infinity]" "[01]" "[1,]" "{\"a\":1,}"
             "\"a\u0001b\"" "\"a\nb\"" "\"\\x41\"" "\ufeff[1]"]]
    (is (= ::json/malformed (refused s)) (pr-str s)))
  (testing "negative control: cheshire's own defaults accept a raw control character"
    (is (= "a\u0001b" (cheshire/parse-string "\"a\u0001b\"")))))

(deftest a-caller-binding-of-the-cheshire-factory-relaxes-nothing
  (binding [factory/*json-factory* (factory/make-json-factory {:allow-comments true
                                                               :allow-single-quotes true
                                                               :strict-duplicate-detection false})]
    (is (= [1] (cheshire/parse-string "[1 /* c */]")) "negative control: the binding is in effect")
    (is (= ::json/malformed (refused "[1 /* c */]")))
    (is (= ::json/malformed (refused "['a']")))
    (is (= ::json/duplicate-key (refused "{\"a\":1,\"a\":2}")))))

(deftest duplicate-keys-are-refused-unless-allowed
  (is (= ::json/duplicate-key (refused "{\"a\":1,\"a\":2}")))
  (is (= ::json/duplicate-key (refused "[{\"x\":{\"a\":1,\"b\":2,\"a\":3}}]")))
  (is (= ::json/duplicate-key (refused "{\"a\":1,\"a\":2}" {:keys :keyword})))
  (is (= {"a" 2} (json/parse-string "{\"a\":1,\"a\":2}" {:allow-duplicate-keys? true})))
  (testing "negative controls: cheshire keeps the last duplicate silently, and equal keys in sibling objects are no duplicate"
    (is (= {"a" 2} (cheshire/parse-string "{\"a\":1,\"a\":2}")))
    (is (= [{"a" 1} {"a" 2}] (json/parse-string "[{\"a\":1},{\"a\":2}]")))))

(deftest keys-arrive-as-the-caller-chooses
  (let [text "{\"a\":{\"b c\":1},\"d\":2}"]
    (is (= {"a" {"b c" 1} "d" 2} (json/parse-string text)))
    (is (= {:a {(keyword "b c") 1} :d 2} (json/parse-string text {:keys :keyword})))
    (is (= {:a {"b c" 1} "d" 2} (json/parse-string text {:keys #{"a"}})))
    (is (= {"A" {"B C" 1} "D" 2} (json/parse-string text {:keys str/upper-case})))
    (let [e (thrown #(json/parse-string text {:keys (fn [_] (throw (ex-info "own failure" {})))}))]
      (is (= ::json/key-failed (:type (ex-data e))))
      (is (= "own failure" (ex-message (ex-cause e))) "the caller's own exception is the cause"))))

(deftest expect-checks-the-value
  (is (= {"a" 1} (json/parse-string "{\"a\":1}" {:expect map?})))
  (is (= ::json/unexpected (refused "[1]" {:expect map?})))
  (is (= "an array" (:found (ex-data (thrown #(json/parse-string "[1]" {:expect map?})))))))

(deftest options-are-checked
  (is (= ::json/invalid-options (refused "1" {:bogus 1})))
  (is (= ::json/invalid-options (refused "1" {:pretty? true})) "a write option is unknown to a read")
  (is (= ::json/invalid-options (refused "1" [:max-bytes 1])))
  (doseq [[k v] [[:max-bytes 0] [:max-bytes Integer/MAX_VALUE] [:max-depth -1] [:max-number-chars 1.5]
                 [:keys :symbol] [:keys #{:a}] [:allow-duplicate-keys? nil] [:one-value? "yes"] [:expect "x"]]]
    (is (= ::json/invalid-options (refused "1" {k v})) (pr-str [k v])))
  (is (= ::json/invalid-options (refusal #(json/write-string 1 {:max-bytes 1}))))
  (is (= ::json/invalid-options (refusal #(json/write-string 1 {:pretty? 1})))))

;; Limits

(deftest the-byte-limit-holds-for-every-entry-point
  (let [at (str "\"" (str/join (repeat 8 "é")) "\"")
        over (str at " ")
        limit (count (.getBytes at "UTF-8"))]
    (is (= 18 limit))
    (is (string? (json/parse-string at {:max-bytes limit})))
    (is (= ::json/too-large (refused over {:max-bytes limit})))
    (is (= ::json/too-large (refused at {:max-bytes (dec limit)})) "the limit counts UTF-8 bytes, not characters")
    (is (= ::json/too-large (refusal #(json/parse-bytes (.getBytes over "UTF-8") {:max-bytes limit}))))
    (is (= ::json/too-large (refusal #(json/parse-stream (ByteArrayInputStream. (.getBytes over "UTF-8"))
                                                         {:max-bytes limit}))))
    (is (= 1048576 (:limit (ex-data (thrown #(json/parse-string (str/join (repeat 1048577 " ")))))))
        "1 MiB by default")))

(deftest an-endless-stream-is-refused-after-one-byte-past-the-limit
  (let [read (atom 0)
        in (proxy [InputStream] []
             (read
               ([] (swap! read inc) 32)
               ([^bytes b] (let [n (alength b)] (swap! read + n) (java.util.Arrays/fill b (byte 32)) n))
               ([^bytes b off len] (swap! read + len) (java.util.Arrays/fill b (int off) (int (+ off len)) (byte 32)) len)))]
    (is (= ::json/too-large (refusal #(json/parse-stream in {:max-bytes 1000}))))
    (is (<= 1001 @read 9000))))

(deftest only-regular-files-are-read
  (with-temp-dir*
    (fn [dir]
      (let [fifo (str (fs/path dir "fifo"))]
        (is (= ::json/not-a-file (refusal #(json/parse-file dir))))
        (is (= ::json/not-a-file (refusal #(json/parse-file (fs/path dir "missing")))))
        (p/shell "mkfifo" fifo)
        (is (= ::json/not-a-file (refusal #(json/parse-file fifo))) "a FIFO cannot block the read")
        (fs/write-bytes (fs/path dir "bad.json") (.getBytes "[1," "UTF-8"))
        (let [e (thrown #(json/parse-file (fs/path dir "bad.json")))]
          (is (= ::json/malformed (:type (ex-data e))))
          (is (= (str (fs/path dir "bad.json")) (:file (ex-data e)))))))))

(deftest invalid-utf-8-is-refused
  (is (= ::json/invalid-utf8 (refusal #(json/parse-bytes (bytes-of [0x22 0xC3 0x22])))))
  (is (= ::json/invalid-utf8 (refusal #(json/parse-bytes (bytes-of [0x22 0xED 0xA0 0x80 0x22]))))
      "an encoded surrogate is not UTF-8")
  (is (= ::json/invalid-utf8 (refused "\"\ud800\"")) "a string holding a lone surrogate has no UTF-8 encoding")
  (testing "negative controls: valid multi-byte text and an escaped lone surrogate read"
    (is (= "é日" (json/parse-bytes (.getBytes "\"é日\"" "UTF-8"))))
    (is (= "\ud800" (json/parse-string "\"\\ud800\"")))))

(deftest nesting-deeper-than-the-limit-is-refused
  (is (vector? (json/parse-string (nest 64 "1"))))
  (is (= ::json/too-deep (refused (nest 65 "1"))))
  (is (= 64 (:limit (ex-data (thrown #(json/parse-string (nest 65 "1")))))))
  (is (= ::json/too-deep (refused (str "{\"a\":" (nest 3 "{}") "}") {:max-depth 3})))
  (is (map? (json/parse-string (str "{\"a\":" (nest 2 "{}") "}") {:max-depth 4})))
  (is (= ::json/too-deep (refused (nest 200000 ""))) "far too deep never reaches the recursive builder")
  (testing "negative control: brackets inside strings do not nest"
    (is (= ["[[[[[["] (json/parse-string "[\"[[[[[[\"]" {:max-depth 1})))))

(deftest long-number-literals-are-refused
  (let [digits (fn [n] (str/join (repeat n "1")))]
    (is (number? (json/parse-string (digits 100))))
    (is (= ::json/number-too-long (refused (digits 101))))
    (is (= ::json/number-too-long (refused (str "-" (digits 100)))) "the sign counts")
    (is (= ::json/number-too-long (refused (str "1." (digits 99)))) "the point counts")
    (is (= ::json/number-too-long (refused (str "[0.5," (digits 3) "]") {:max-number-chars 2})))
    (is (= ::json/number-too-long (refused "12.34" {:max-number-chars 4}))
        "the whole literal counts, while Jackson bounds integer and fraction separately")
    (testing "negative controls: digits in a string and in a key are no number"
      (is (string? (json/parse-string (str "\"" (digits 500) "\""))))
      (is (map? (json/parse-string (str "{\"" (digits 500) "\":1}")))))
    (testing "Jackson's limit refuses a long integer when the scan is bypassed"
      (with-redefs-fn {#'json/check-numbers! (fn [_ _] nil)}
        #(is (= ::json/number-too-long (refused (digits 101))))))))

(deftest a-number-that-overflows-a-double-is-refused
  (doseq [s ["1e400" "-1e400" "[1.5E+309]" "{\"a\":[2e308]}"]]
    (is (= ::json/number-out-of-range (refused s)) s))
  (is (= ::json/number-out-of-range
         (refused (str (str/join (repeat 310 "9")) ".0") {:max-number-chars 400}))
      "a long decimal without an exponent overflows too")
  (testing "negative controls"
    (is (= 1.0E308 (json/parse-string "1e308")))
    (is (= ["1e400"] (json/parse-string "[\"1e400\"]")) "a string is no number")
    (is (= (bigint (str/join (repeat 310 "9"))) (json/parse-string (str/join (repeat 310 "9")) {:max-number-chars 400}))
        "an integer never overflows")
    (is (= ##Inf (cheshire/parse-string "1e400")) "cheshire alone reads Infinity")))

(deftest an-unterminated-string-ends-the-number-scan-in-linear-time
  (let [evil (str "[\"" (str/join (repeat 200000 "\\\"")) "," (str/join (repeat 101 "1")) "]")
        start (System/nanoTime)]
    (is (= ::json/malformed (refused evil)))
    (is (< (/ (- (System/nanoTime) start) 1e6) 5000))))

;; Refusals carry no content

(def ^:private secret "hunter2secret0123")

(defn- carries-secret?
  [^Throwable e]
  (boolean (some (fn [^Throwable x]
                   (or (str/includes? (str (ex-message x)) secret)
                       (str/includes? (pr-str (ex-data x)) secret)))
                 (take-while some? (iterate ex-cause e)))))

(deftest no-refusal-carries-the-input
  (doseq [s [(str "{\"a\":" secret "}")
             (str "{\"" secret "\":1,\"" secret "\":2}")
             (str "[1] " secret)
             (str "[\"" secret "\u0001\"]")]]
    (let [e (thrown #(json/parse-string s))]
      (is (json/refusal? e) s)
      (is (not (carries-secret? e)) s)
      (is (integer? (or (:line (ex-data e)) 1)))))
  (let [e (thrown #(json/parse-string (str "{\"a\":" secret "}")))]
    (is (= {:line 1 :column 6} (select-keys (ex-data e) [:line :column])) "a position stands in for the content"))
  (testing "negative control: cheshire's own exception quotes the token"
    (is (carries-secret? (thrown #(cheshire/parse-string (str "{\"a\":" secret "}"))))))
  (testing "a value the writer cannot encode is not printed"
    (let [v (proxy [Object] [] (toString [] secret))]
      (is (carries-secret? (thrown #(cheshire/generate-string {:a v}))) "negative control")
      (let [e (thrown #(json/write-string {:a v}))]
        (is (= ::json/not-encodable (:type (ex-data e))))
        (is (not (carries-secret? e)))))))

;; Golden vectors of gumshoe's parse-strict: a wrapper of gumshoe's shape
;; over briko.json gives the same answers.

(defn- parse-strict
  [text]
  (when-not (str/blank? text)
    (json/parse-string text {:keys :keyword})))

(defn- parse-or-nil
  [text]
  (try (parse-strict text) (catch Exception _ nil)))

(deftest gumshoe-parse-strict-vectors
  (is (= [{:a 1} {:b [2 3]}] (parse-strict "[{\"a\":1},{\"b\":[2,3]}]")))
  (is (vector? (parse-strict "[1,2]")))
  (is (= {:a 1} (parse-strict "  {\"a\":1}\n")))
  (is (nil? (parse-strict "")))
  (is (nil? (parse-strict nil)))
  (is (nil? (parse-strict "null")))
  (doseq [s ["[{\"time\":\"2026-01-01T00:00:00Z\"}, {\"ti" "[1,2" "{\"a\":[1,{\"b\":2}"
             "[1] trailing" "[1] null" "{\"a\":1}{\"b\":2}" "repository abc opened (version 2)\n[]"]]
    (is (json/refusal? (thrown #(parse-strict s))) s))
  (is (nil? (parse-or-nil "[1,2")))
  (is (nil? (parse-or-nil "[1] x")))
  (is (= [1 2] (parse-or-nil "[1,2]"))))

;; Writing

(defn- code-point-string [& cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps] (.appendCodePoint sb (int cp)))
    (str sb)))

(def ^:private hostile-parts
  "One sample of every class hidden? names: C0 (ESC with a screen clear, BEL,
   NUL), DEL, C1 (the 8-bit CSI, NEL), the bidirectional embeddings, overrides,
   isolates and marks, the Arabic letter mark, zero-width space, joiners and
   word joiner, the byte order mark, the soft hyphen, line and paragraph
   separators, a variation selector, a Hangul filler, a tag character, a lone
   high and a lone low surrogate, an unassigned code point and a noncharacter."
  [(str "\u001b" (char 0x5B) "2J") "\u0007" "\u0000" "\u007f" "\u009b" "\u0085"
   "\u202a" "\u202b" "\u202c" "\u202d" "\u202e"
   "\u2066" "\u2067" "\u2068" "\u2069" "\u200e" "\u200f" "\u061c"
   "\u200b" "\u200c" "\u200d" "\u2060" "\ufeff" "\u00ad"
   "\u2028" "\u2029" "\ufe0f" "\u3164"
   (code-point-string 0xE0041)
   "\ud800" "\udc00" "\u0378" "\uffff"])

(def ^:private hostile
  (str "evil " (str/join "x" hostile-parts) " end"))

(def ^:private readable
  "Non-ASCII text that is not hidden and stays readable on the wire."
  "Größe 日本 café ✓")

(def ^:private extra
  {:rows [{:name hostile :labels {hostile hostile}}
          {:name readable :note (str readable hostile)}]
   :nested {:deeper [#{hostile} (list hostile)]}
   hostile [hostile]
   (keyword (str "key " hostile)) "keyword key"
   :symbol (symbol hostile)
   :char (char 0x9b)})

(defn- raw-hidden
  "The hidden code points in printed outside the whitespace that separates
   JSON values, as hex strings."
  [^String printed]
  (->> (.toArray (.codePoints printed))
       (filter json/hidden?)
       (remove #{9 10 13})
       (map #(format "U+%04X" %))
       distinct
       vec))

(deftest every-sample-is-hidden
  (doseq [part hostile-parts
          :let [cps (.toArray (.codePoints ^String part))]]
    (is (some json/hidden? cps) (pr-str part)))
  (testing "readable non-ASCII text is not hidden"
    (is (not-any? json/hidden? (.toArray (.codePoints ^String readable))))))

(def ^:private lone-surrogate-keys-read?
  "Whether this runtime's Jackson reads an escaped lone surrogate in an
   object key: the one in babashka v1.13.229 does, jackson-core 2.23 refuses
   it (\"Broken surrogate pair in field name\"), in a value both read it."
  (try (map? (cheshire/parse-string "{\"\\ud800\":1}")) (catch Exception _ false)))

(defn- lone-surrogate?
  "Whether s holds a surrogate that is not half of a pair."
  [^String s]
  (boolean (some #(<= 0xD800 % 0xDFFF) (.toArray (.codePoints s)))))

(deftest writes-no-hidden-code-point-and-reads-back-whole
  (let [out (json/write-string extra {:pretty? true})]
    (is (= [] (raw-hidden out)))
    (testing "the bytes on the wire hold none either: a lone surrogate is escaped, not replaced"
      (is (= out (String. (.getBytes ^String out "UTF-8") "UTF-8"))))
    (testing "readable non-ASCII text stays as it is"
      (is (str/includes? out readable)))
    (if lone-surrogate-keys-read?
      (let [read (cheshire/parse-string out)]
        (testing "a JSON reader gets back exactly what cheshire would have encoded"
          (is (= (cheshire/parse-string (cheshire/generate-string extra)) read))
          (is (= read (json/parse-string out)) "briko.json reads its own output")
          (is (= hostile (get-in read ["rows" 0 "name"])))
          (is (= hostile (get-in read ["rows" 0 "labels" hostile])))
          (is (= [hostile] (get read hostile)))
          (is (= "\u009b" (get read "char")))))
      (testing "this Jackson refuses the escaped lone surrogate in a key, and briko.json says so"
        (is (= ::json/malformed (refused out)))))))

(deftest clean-data-prints-as-cheshire-prints-it
  (let [data {:findings [{:severity :info :summary readable}]
              :rows [{:name "web" :n 1 :at #inst "2026-11-28T00:00:00Z"}]
              :c \a :q "say \"hi\"\n" :s 'sym}]
    (is (= (cheshire/generate-string data {:pretty true}) (json/write-string data {:pretty? true})))
    (is (= (cheshire/generate-string data) (json/write-string data)))))

(deftest a-backslash-or-quote-in-text-does-not-confuse-the-escaper
  (let [data {"a\\" "\\\u202e\"" :b "\"\\u202e\""}]
    (is (= (cheshire/parse-string (cheshire/generate-string data))
           (cheshire/parse-string (json/write-string data {:pretty? true}))))
    (is (= [] (raw-hidden (json/write-string data))))))

(deftest escape-hidden-works-on-any-json-text
  (is (= "[\"a\\u202Eb\"]" (json/escape-hidden "[\"a\u202eb\"]")))
  (is (= "\"\\uDB40\\uDC41\"" (json/escape-hidden (str "\"" (code-point-string 0xE0041) "\""))) "a surrogate pair")
  (is (= "[1,\n\t2]" (json/escape-hidden "[1,\n\t2]")) "whitespace between values stays")
  (is (= "\"Größe\"" (json/escape-hidden "\"Größe\""))))

(deftest plain-writers-fail-these-checks
  (testing "cheshire alone writes hidden code points raw, so the checks above can fail"
    (is (seq (raw-hidden (cheshire/generate-string extra {:pretty true})))))
  (testing "write-string without the escaper would fail them"
    (with-redefs-fn {#'json/escape-hidden identity}
      #(is (seq (raw-hidden (json/write-string extra)))))))

(deftest keys-that-print-to-the-same-name-are-refused
  (doseq [m [{:a 1 "a" 2} {'a 1 "a" 2} {1 :x "1" :y} {nil 1 "" 2} {:x/y 1 "x/y" 2}]]
    (is (= ::json/duplicate-key (refusal #(json/write-string [{:ok m}]))) (pr-str m)))
  (is (= "{\"a\":1,\"a\":2}" (json/write-string (array-map :a 1 "a" 2) {:allow-duplicate-keys? true})))
  (testing "negative controls: distinct names, and equal names in sibling maps"
    (is (= "{\"a\":1,\"b\":2}" (json/write-string (array-map :a 1 "b" 2))))
    (is (= "[{\"a\":1},{\"a\":2}]" (json/write-string [{:a 1} {"a" 2}])))
    (is (= "{\"a\":1,\"a\":2}" (cheshire/generate-string (array-map :a 1 "a" 2))) "cheshire alone writes both")))

(deftest a-value-nested-beyond-the-limit-is-refused-before-printing
  (let [nested (fn [n] (reduce (fn [a _] [a]) 1 (range n)))]
    (is (string? (json/write-string (nested 256))))
    (is (= ::json/too-deep (refusal #(json/write-string (nested 257)))))
    (is (= ::json/too-deep (refusal #(json/write-string {:a (list {:b (nested 2)})} {:max-depth 4}))))
    (is (string? (json/write-string {:a (list {:b (nested 1)})} {:max-depth 4})))
    (is (= ::json/too-deep (refusal #(json/write-string (nested 100000) {:max-depth 1000000})))
        "the printer's own limit or stack refuses what the walk lets through")))

(def ^:private any-code-point
  "Any code point, surrogates included, so a generated string can hold a lone
   surrogate; weighted towards the hidden classes and the edges of ASCII."
  (gen/frequency [[3 (gen/choose 0 0x7f)]
                  [2 (gen/elements (mapcat #(.toArray (.codePoints ^String %)) hostile-parts))]
                  [1 (gen/elements [(int \") (int \\) (int \u) 0x2028 0xe0001])]
                  [2 (gen/choose 0x80 0xffff)]
                  [1 (gen/choose 0x10000 0x10ffff)]]))

(def ^:private any-string
  (gen/fmap #(apply code-point-string %) (gen/vector any-code-point 0 24)))

(def ^:private round-trips
  (prop/for-all [ks (gen/vector any-string 0 4)
                 vs (gen/vector any-string 0 4)]
    (let [data {:extra (zipmap ks vs) :list vs}
          out (json/write-string data {:pretty? true})]
      (and (= [] (raw-hidden out))
           (or (and (not lone-surrogate-keys-read?) (some lone-surrogate? ks))
               (and (= (cheshire/parse-string (cheshire/generate-string data)) (cheshire/parse-string out))
                    (= (cheshire/parse-string out) (json/parse-string out))))))))

(deftest any-string-round-trips-without-raw-hidden-code-points
  (let [result (tc/quick-check 300 round-trips :seed 14)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))
