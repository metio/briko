;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json-fast-path-test
  "The fast paths of briko.json refuse exactly what the exact checks
   refuse: the number screen and the character loop against the regex
   token scan, the UTF-8 count against the JDK encoder, the shortened
   hidden-code-point search against the full one, and the level-wise
   write walk against its rules."
  (:require [briko.json :as json]
            [briko.json-fixtures :as fixtures]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.nio.charset StandardCharsets)))

(def ^:private babashka? (some? (System/getProperty "babashka.version")))

(defn- outcome
  "[:ok value] or [:refused type] of (f)."
  [f]
  (try
    [:ok (f)]
    (catch clojure.lang.ExceptionInfo e
      (if (json/refusal? e) [:refused (:type (ex-data e))] [:foreign (ex-message e)]))))

(defn- exact
  "(f) with every fast path off: the JDK encoder counts the bytes and the
   regex token scan judges every number, whatever the runtime."
  [f]
  (with-redefs-fn {#'json/babashka? true
                   #'json/screened? (fn [_ _] false)}
    f))

(defn- zeros [n] (str/join (repeat n "0")))

;; Generated texts

(def ^:private hex-string
  (gen/fmap #(str/join (map (fn [i] (nth "0123456789abcdef" i)) %))
            (gen/vector (gen/choose 0 15) 1 40)))

(def ^:private digits
  (gen/fmap str/join (gen/vector (gen/elements "0123456789") 1 12)))

(def ^:private number-literal
  (gen/let [sign (gen/elements ["" "-"])
            int-part (gen/frequency [[6 digits]
                                     [1 (gen/fmap #(str "1" (zeros %)) (gen/choose 95 260))]])
            frac (gen/elements ["" ".5" ".25" (str "." (zeros 120))])
            exp (gen/elements ["" "e5" "E+12" "e-7" "e99" "e308" "e309" "e400" "E-400" "e0400" "e1234" "e+999"])]
    (str sign int-part frac exp)))

(def ^:private string-content
  (gen/one-of [hex-string
               (gen/fmap #(str (java.util.UUID. (first %) (second %))) (gen/tuple gen/large-integer gen/large-integer))
               (gen/elements ["3e400" "1e999" "d3e249ff" "sha256:0e752" "\\\"" "\\\\" "\\u00e9" "\\uE000" "é"
                              "\uD83D\uDE80" "a, 1e400" "[1e400]" ": 2e308" (str "9" (zeros 150))])
               number-literal]))

(def ^:private fragment
  (gen/frequency [[4 (gen/elements ["{" "}" "[" "]" ":" "," " " "\n" "true" "false" "null"])]
                  [1 (gen/elements ["-" "." "e" "E" "+" "\"" "\\" "\uD800" "\uDC00" "\uD83D\uDE80" "\u0001"])]
                  [5 number-literal]
                  [5 (gen/fmap #(str "\"" (str/join %) "\"") (gen/vector string-content 0 3))]]))

(def ^:private any-text
  (gen/fmap str/join (gen/vector fragment 0 12)))

(def ^:private json-text
  "Valid JSON more often than not: an array or object of numbers and
   strings that look like numbers."
  (gen/let [values (gen/vector (gen/one-of [number-literal
                                            (gen/fmap #(str "\"" (str/join %) "\"") (gen/vector string-content 0 3))])
                               0 8)
            object? gen/boolean]
    (if object?
      (str "{" (str/join "," (map-indexed (fn [i v] (str "\"k" i "e123\":" v)) values)) "}")
      (str "[" (str/join ", " values) "]"))))

(def ^:private limits [3 10 100 250])

(def ^:private reads-as-the-exact-checks
  (prop/for-all [s (gen/one-of [any-text json-text])
                 limit (gen/elements limits)
                 one-value? gen/boolean]
    (let [opts {:max-number-chars limit :one-value? one-value?}]
      (and (= (outcome #(exact (fn [] (json/parse-string s opts))))
              (outcome #(json/parse-string s opts)))
           (= (outcome #(exact (fn [] (json/parse-bytes (.getBytes ^String s StandardCharsets/UTF_8) opts))))
              (outcome #(json/parse-bytes (.getBytes ^String s StandardCharsets/UTF_8) opts)))))))

(deftest every-path-refuses-what-the-exact-checks-refuse
  (let [result (tc/quick-check 600 reads-as-the-exact-checks :seed 20261128)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))

(def ^:private scans-agree
  (prop/for-all [s (gen/one-of [any-text json-text])
                 limit (gen/elements limits)]
    (let [opts {:max-number-chars limit}
          regex-scan (outcome #(#'json/check-numbers! s opts))
          loop-scan (outcome #(#'json/scan-numbers! s opts))]
      (and (= regex-scan loop-scan)
           ;; A text the screen walks needs no scan and is valid UTF-8.
           (or (not (#'json/screened? s limit))
               (and (= [:ok nil] regex-scan)
                    (not (neg? (#'json/utf-8-count s)))))))))

(deftest the-loop-scan-and-the-screen-agree-with-the-regex-scan
  (let [result (tc/quick-check 600 scans-agree :seed 20261129)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))

(def ^:private any-chars
  (gen/fmap #(String. (char-array (map char %)))
            (gen/vector (gen/frequency [[4 (gen/choose 0 0x7f)]
                                        [2 (gen/choose 0x80 0xffff)]
                                        [2 (gen/choose 0xd800 0xdfff)]])
                        0 30)))

(def ^:private counts-like-the-encoder
  (prop/for-all [s any-chars]
    (let [encoded (outcome #(exact (fn [] (#'json/utf-8-length s))))
          counted (outcome #(with-redefs-fn {#'json/babashka? false} (fn [] (#'json/utf-8-length s))))]
      (= encoded counted))))

(deftest the-utf-8-count-agrees-with-the-encoder
  (let [result (tc/quick-check 500 counts-like-the-encoder :seed 7)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed]))))
  (is (= 4 (#'json/utf-8-count "\uD83D\uDE80")))
  (is (= -1 (#'json/utf-8-count "a\uD83Db")) "a high surrogate without its low half")
  (is (= -1 (#'json/utf-8-count "\uDE80\uD83D")) "the halves in the wrong order"))

;; Documents as they come from kubectl, npm and GitHub

(defn- documents
  []
  [(cheshire/generate-string (fixtures/pod-list 1 20))
   (cheshire/generate-string (fixtures/npm-document 2 40))
   (cheshire/generate-string (fixtures/release-list 3 20))])

(deftest generated-documents-read-as-cheshire-reads-them
  (doseq [text (documents)]
    (is (= (cheshire/parse-string text) (json/parse-string text {:max-bytes 16777216})))
    (is (= (cheshire/parse-string text) (exact #(json/parse-string text {:max-bytes 16777216}))))))

(deftest hex-strings-uuids-and-digit-keys-skip-the-token-scan-under-babashka
  (let [scans (atom 0)
        counting (fn [_ _] (swap! scans inc) nil)
        doc (str "{\"3e400\":\"1e999\",\"d3e249ff-8af0-4c1e-9e00-1e2b3c4d5e6f\":"
                 "[\"sha256:0e752aa\",\"\\uE000\",\"a, 2e308\",\"" (str/join (repeat 150 "9")) "\"],"
                 "\"k1\":[1,-2,3.5,1e5,2E-7,1.25e+99]}")]
    (with-redefs-fn {#'json/check-numbers! counting}
      (fn []
        (doseq [text (conj (documents) doc)]
          (is (some? (json/parse-string text {:max-bytes 16777216}))))
        (when babashka?
          (is (zero? @scans) "the screen walks every one of them"))
        (testing "negative control: a number of three exponent digits sends the text into the scan"
          (reset! scans 0)
          (is (= {"a" 1.0E300} (json/parse-string "{\"a\":1e300}")))
          (when babashka?
            (is (= 1 @scans))))))
    (testing "the screen walks them to the end"
      (doseq [text (conj (documents) doc)]
        (is (#'json/screened? text 100)))
      (is (not (#'json/screened? "{\"a\":1e300}" 100)))
      (is (not (#'json/screened? (str "[\"a\",\"" "\uD800" "\"]") 100)) "a surrogate standing alone stops it")
      (is (not (#'json/screened? (str/join (repeat 101 "1")) 100)) "a literal over the limit stops it")
      (is (#'json/screened? (str/join (repeat 100 "1")) 100)))))

(deftest refusals-in-documents-full-of-hex-strings
  (let [text (cheshire/generate-string (fixtures/pod-list 5 3))
        with-member (fn [literal] (str (subs text 0 (dec (count text))) ",\"x\":" literal "}"))]
    (doseq [[s type] [[(with-member "1e400") ::json/number-out-of-range]
                      [(with-member (str/join (repeat 101 "7"))) ::json/number-too-long]
                      [(with-member "-1.5E+309") ::json/number-out-of-range]
                      [(str text " 1e400") ::json/number-out-of-range]
                      [(str (subs text 1) ",\"x\":1e400}") ::json/number-out-of-range]
                      [(with-member "1e300") :no-refusal]]]
      (let [expected (if (= :no-refusal type) :ok [:refused type])
            got (outcome #(json/parse-string s))]
        (is (= expected (if (= :ok (first got)) :ok got)) type)
        (is (= got (outcome #(exact (fn [] (json/parse-string s))))) type)))))

(deftest a-long-mantissa-with-a-short-exponent-that-overflows-is-refused
  (let [overflows (str "1" (zeros 210) "e99")]
    (doseq [limit [220 300 400]]
      (is (= [:refused ::json/number-out-of-range]
             (outcome #(json/parse-string overflows {:max-number-chars limit})))
          limit))
    (testing "negative control: one that stays below Double/MAX_VALUE reads"
      (is (= 1.0E299 (json/parse-string (str "1" (zeros 200) "e99") {:max-number-chars 400}))))))

;; Writing

(defn- reference-escape
  "escape-hidden as a single search with the full class, ASCII first."
  [^String printed]
  (str/replace printed
               (re-pattern (str "[" @#'json/hidden-class "&&[^\\t\\n\\r]]"))
               (fn [^String s] (str/join (map #(format "\\u%04X" (int %)) s)))))

(def ^:private escapes-like-the-full-search
  (prop/for-all [s any-chars]
    (= (reference-escape s) (json/escape-hidden s))))

(deftest the-shortened-search-escapes-what-the-full-one-escapes
  (let [result (tc/quick-check 1000 escapes-like-the-full-search :seed 3)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed]))))
  (is (= "\"\\uD83Dx\\uDE80\"" (json/escape-hidden "\"\uD83Dx\uDE80\""))
      "two lone surrogates around ASCII stay lone")
  (is (= "\"\uD83D\uDE80\"" (json/escape-hidden "\"\uD83D\uDE80\"")) "a pair is a printable emoji"))

(deftest keys-of-one-kind-pass-and-colliding-kinds-are-refused
  (is (string? (json/write-string {"a" 1 "b" 2})))
  (is (string? (json/write-string {(keyword nil "x/y") 1 :y 2})) "keywords without a namespace print their whole name")
  (is (= [:refused ::json/duplicate-key] (outcome #(json/write-string {(keyword nil "x/y") 1 :x/y 2}))))
  (is (= [:refused ::json/duplicate-key] (outcome #(json/write-string [[{"a" 1 :a 2}]]))))
  (is (= [:refused ::json/duplicate-key] (outcome #(json/write-string (java.util.HashMap. ^java.util.Map (array-map "a" 1 :a 2)))))))

(deftest a-value-with-both-faults-is-refused-for-the-shallower-one
  (let [nested (fn [n] (reduce (fn [a _] [a]) 1 (range n)))]
    (is (= [:refused ::json/duplicate-key] (outcome #(json/write-string [(nested 300) {:a 1 "a" 2}]))))
    (is (= [:refused ::json/too-deep] (outcome #(json/write-string [(nested 4) [[[[[{:a 1 "a" 2}]]]]]] {:max-depth 3}))))
    (testing "a scalar beside them needs no step"
      (is (string? (json/write-string (vec (range 100000))))))))

(deftest a-cache-shaped-map-writes-as-cheshire-writes-it
  (let [cache (fixtures/derivation-cache 9 50)]
    (is (= (cheshire/parse-string (cheshire/generate-string cache)) (json/parse-string (json/write-string cache))))))

(defn- disagrees?
  "Whether quick-check found a counterexample by comparison, not by an
   exception thrown inside the property."
  [result]
  (and (not (:pass? result)) (false? (:result result))))

(deftest negative-controls-the-properties-fail-for-a-broken-fast-path
  (testing "a screen that walks every text lets refusals through"
    (with-redefs-fn {#'json/screened? (fn [_ _] true)
                     #'json/babashka? true}
      #(is (disagrees? (tc/quick-check 300 reads-as-the-exact-checks :seed 20261128)))))
  (testing "a loop scan that judges nothing disagrees with the regex scan"
    (with-redefs-fn {#'json/scan-numbers! (fn [_ _] nil)}
      #(is (disagrees? (tc/quick-check 300 scans-agree :seed 20261129)))))
  (testing "a count that ignores surrogates disagrees with the encoder"
    (with-redefs-fn {#'json/utf-8-count (fn [^String s] (alength (.getBytes s StandardCharsets/UTF_8)))}
      #(is (disagrees? (tc/quick-check 300 counts-like-the-encoder :seed 7)))))
  (testing "a search that skips the surrogates misses lone ones"
    (with-redefs-fn {#'json/escape-hidden (fn [s] (reference-escape (str/replace s #"[\x{D800}-\x{DFFF}]" "")))}
      #(is (disagrees? (tc/quick-check 300 escapes-like-the-full-search :seed 3))))))
