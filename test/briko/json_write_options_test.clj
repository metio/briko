;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json-write-options-test
  (:require [briko.json :as json]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (json/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- code-point-string [& cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps] (.appendCodePoint sb (int cp)))
    (str sb)))

(def ^:private krei-printer
  "The pretty-printer options of krei's :json filter (src/krei/facts.clj on
   origin/main, 2026-11-28)."
  {:indentation 2 :object-field-value-separator ": " :indent-arrays? true})

(def ^:private krei-options
  {:indent 2 :key-separator ": " :indent-arrays? true})

(defn- krei-sorted
  "The key order krei's :json filter prints in."
  [x]
  (cond (map? x) (into (sorted-map-by #(compare (name %1) (name %2))) (map (fn [[k v]] [k (krei-sorted v)])) x)
        (sequential? x) (mapv krei-sorted x)
        :else x))

(defn- nested
  [n]
  (reduce (fn [a i] (if (even? i) [a] {:k a})) "leaf" (range n)))

(def ^:private clean
  "Data without hidden code points, as facts hold it."
  {:name "briko" :licenses ["0BSD"] :empty-list [] :empty-map {} :nothing nil
   :yes true :no false :n 42 :x 1.5 :big 12345678901234567890N
   :text "say \"hi\"\n\ttab \\ back / slash" :readable "Größe 日本 café ✓"
   :tool :keyword :nested {:a [[1 2] [] [{:b {:c [3]}}]] :z {"s" "t"}}})

(def ^:private hidden-samples
  "Control characters (NUL, BEL, ESC, every C0 escape JSON names, DEL, the
   8-bit CSI, NEL), zero-width space, a right-to-left override, the line
   separator, a byte order mark, a tag character and a lone surrogate."
  [(code-point-string 0) (code-point-string 7) (code-point-string 0x1b) "\b\f\n\r\t"
   (code-point-string 0x7f) (code-point-string 0x9b) (code-point-string 0x85)
   "\u200b" "\u202e" "\u2028" "\ufeff" (code-point-string 0xE0041) "\ud800"])

(def ^:private hostile
  (str "a" (str/join "b" hidden-samples) "c"))

(def ^:private hostile-key
  "hostile without the lone surrogate, which jackson-core 2.23 refuses to
   read back in an object key."
  (str/replace hostile "\ud800" ""))

(deftest krei-options-print-as-cheshire-prints-them
  (doseq [x [clean (krei-sorted clean) [] {} [[]] [{}] "s" 1 nil (nested 200)]]
    (is (= (cheshire/generate-string x {:pretty krei-printer})
           (json/write-string x krei-options))
        (pr-str x)))
  (testing "krei's own call, a sorted map with the printer options as a map"
    (is (= (cheshire/generate-string (krei-sorted clean) {:pretty krei-printer})
           (json/write-string (krei-sorted clean) krei-options))))
  (testing "negative control: the default pretty output differs from krei's"
    (is (not= (cheshire/generate-string clean {:pretty krei-printer})
              (json/write-string clean {:pretty? true})))))

(deftest krei-options-with-hidden-code-points
  (let [x {:title hostile :list [hostile {hostile-key hostile}]}]
    (testing ":escape-hidden? false writes the bytes cheshire writes"
      (is (= (cheshire/generate-string x {:pretty krei-printer})
             (json/write-string x (assoc krei-options :escape-hidden? false)))))
    (testing "the default escapes them: other bytes, the same value"
      (let [out (json/write-string x krei-options)]
        (is (not= (cheshire/generate-string x {:pretty krei-printer}) out))
        (is (str/includes? out "\\u200B"))
        (is (not (str/includes? out "\u200b")))
        (is (= (cheshire/parse-string (cheshire/generate-string x)) (cheshire/parse-string out)))))))

(deftest every-layout-prints-as-cheshire-prints-it
  (doseq [indent (range 0 (inc json/max-indent))
          sep json/key-separators
          arrays? [true false]]
    (is (= (cheshire/generate-string clean {:pretty {:indentation indent
                                                     :object-field-value-separator sep
                                                     :indent-arrays? arrays?}})
           (json/write-string clean {:indent indent :key-separator sep :indent-arrays? arrays?}))
        (pr-str [indent sep arrays?])))
  (testing ":pretty? true alone prints as cheshire's :pretty true"
    (is (= (cheshire/generate-string clean {:pretty true}) (json/write-string clean {:pretty? true})))
    (is (= (cheshire/generate-string clean {:pretty true})
           (json/write-string clean {:pretty? true :indent 2 :key-separator " : " :indent-arrays? false}))))
  (testing ":indent 0 breaks lines without indenting them"
    (is (= "{\n\"a\": [\n1\n]\n}" (json/write-string {:a [1]} {:indent 0 :key-separator ": " :indent-arrays? true}))))
  (testing "compact output stays compact"
    (is (= "{\"a\":[1,{\"b\":2}]}" (json/write-string {:a [1 {:b 2}]})))
    (is (= (cheshire/generate-string clean) (json/write-string clean)))))

(deftest escape-hidden-false-prints-as-plain-cheshire
  (doseq [s (conj hidden-samples hostile)
          x [s [s] {s s} {:a {:b [s]}}]]
    (is (= (cheshire/generate-string x) (json/write-string x {:escape-hidden? false})) (pr-str x))
    (is (= (cheshire/generate-string x {:pretty true}) (json/write-string x {:escape-hidden? false :pretty? true}))
        (pr-str x)))
  (testing "deep nesting"
    (let [x (nested 255)]
      (is (= (cheshire/generate-string x) (json/write-string x {:escape-hidden? false})))))
  (testing "U+200B and C1 stay raw, C0 gets the escapes JSON requires"
    (is (= "\"a\u200bb\"" (json/write-string "a\u200bb" {:escape-hidden? false})))
    (is (= (str "\"" (code-point-string 0x9b) "\\u001B\\n\"")
           (json/write-string (str (code-point-string 0x9b) (code-point-string 0x1b) "\n") {:escape-hidden? false}))))
  (testing "negative control: the default escapes what false leaves raw"
    (is (= "\"a\\u200Bb\"" (json/write-string "a\u200bb")))
    (is (not= (cheshire/generate-string hostile) (json/write-string hostile)))))

(def ^:private any-code-point
  (gen/frequency [[3 (gen/choose 0 0x7f)]
                  [2 (gen/elements (mapcat #(.toArray (.codePoints ^String %)) hidden-samples))]
                  [2 (gen/choose 0x80 0xffff)]
                  [1 (gen/choose 0x10000 0x10ffff)]]))

(def ^:private any-string
  (gen/fmap #(apply code-point-string %) (gen/vector any-code-point 0 16)))

(def ^:private any-value
  (gen/recursive-gen (fn [inner] (gen/one-of [(gen/vector inner 0 4) (gen/map any-string inner {:max-elements 4})]))
                     (gen/one-of [any-string gen/small-integer gen/boolean (gen/return nil)])))

(def ^:private byte-identical
  (prop/for-all [x any-value]
    (and (= (cheshire/generate-string x) (json/write-string x {:escape-hidden? false}))
         (= (cheshire/generate-string x {:pretty krei-printer})
            (json/write-string x (assoc krei-options :escape-hidden? false))))))

(deftest any-value-prints-as-cheshire-prints-it
  (let [result (tc/quick-check 200 byte-identical :seed 5)]
    (is (true? (:pass? result)) (pr-str (select-keys result [:shrunk :seed])))))

(deftest any-value-differs-from-cheshire-when-escaped
  (testing "negative control: the property can fail, and does with escaping on"
    (let [result (tc/quick-check 200 (prop/for-all [x any-value]
                                       (= (cheshire/generate-string x) (json/write-string x)))
                                 :seed 5)]
      (is (false? (:result result))))))

(defn- thumbprint
  "An RFC 7638 thumbprint written with briko.json: the required members
   in lexicographic order, no whitespace, code points raw."
  [members]
  (let [canon (json/write-string (into (sorted-map) members) {:escape-hidden? false})]
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
                     (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String canon "UTF-8")))))

(deftest an-rfc-7638-thumbprint-can-be-written
  (testing "the Ed25519 example of RFC 8037 section A.3"
    (is (= "kPrK_qmxVWaYVA9wwBF6Iuo3vVzz7TxHCTwXBygrS4k"
           (thumbprint {"crv" "Ed25519" "kty" "OKP" "x" "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"}))))
  (testing "a member holding U+200B is hashed raw, as RFC 7638 spells it"
    (let [members {"crv" "Ed25519\u200b" "kty" "OKP" "x" "11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo"}]
      (is (= "{\"crv\":\"Ed25519\u200b\",\"kty\":\"OKP\",\"x\":\"11qYAYKxCrfVS_7TyWQHOg7hcvPapiMlrwIaaPcHURo\"}"
             (json/write-string (into (sorted-map) members) {:escape-hidden? false})))
      (is (not= (thumbprint members)
                (thumbprint (assoc members "crv" "Ed25519")))
          "negative control: the hidden code point reaches the hash"))))

(deftest invalid-write-options-are-refused
  (doseq [opts [{:indent -1} {:indent 17} {:indent 2.0} {:indent "2"} {:indent 1000000000}
                {:pretty? true :key-separator "="} {:pretty? true :key-separator " :  "}
                {:pretty? true :key-separator ":\n"} {:pretty? true :key-separator nil}
                {:pretty? true :indent-arrays? "yes"} {:pretty? true :indent-arrays? nil}
                {:escape-hidden? nil} {:escape-hidden? "false"}
                {:indent 2 :pretty? false}
                {:key-separator ": "} {:indent-arrays? true} {:pretty? false :indent-arrays? false}
                {:indentation 2}]]
    (is (= ::json/invalid-options (refusal #(json/write-string {:a 1} opts))) (pr-str opts)))
  (testing "negative controls: the edges and combinations that are allowed"
    (doseq [opts [{:indent 0} {:indent 16} {:indent nil} {:indent 2 :pretty? true}
                  {:pretty? true :key-separator ":"} {:pretty? true :indent-arrays? true}
                  {:escape-hidden? false} {:escape-hidden? true :pretty? false}]]
      (is (string? (json/write-string {:a 1} opts)) (pr-str opts))))
  (testing "a read refuses the write options"
    (is (= ::json/invalid-options (refusal #(json/parse-string "1" {:indent 2}))))
    (is (= ::json/invalid-options (refusal #(json/parse-string "1" {:escape-hidden? false}))))))

(deftest the-write-refusals-hold-with-every-option
  (doseq [opts [krei-options (assoc krei-options :escape-hidden? false) {:escape-hidden? false}]]
    (is (= ::json/duplicate-key (refusal #(json/write-string {:a 1 "a" 2} opts))) (pr-str opts))
    (is (= ::json/too-deep (refusal #(json/write-string (nested 257) opts))) (pr-str opts))
    (is (= ::json/too-deep (refusal #(json/write-string (nested 5) (assoc opts :max-depth 4)))) (pr-str opts))
    (testing "negative controls"
      (is (string? (json/write-string (nested 256) opts)))
      (is (string? (json/write-string (array-map :a 1 "a" 2) (assoc opts :allow-duplicate-keys? true)))))))
