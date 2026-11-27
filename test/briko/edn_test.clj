;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.edn-test
  (:require [babashka.fs :as fs]
            [briko.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (java.io ByteArrayInputStream)
           (java.time Instant)
           (java.util UUID)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (edn/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- refused
  ([s] (refusal #(edn/read-string s)))
  ([s opts] (refusal #(edn/read-string s opts))))

(defn- nest
  [depth open close inner]
  (str (str/join (repeat depth open)) inner (str/join (repeat depth close))))

(defn- bytes-of
  ^bytes [xs]
  (byte-array (map unchecked-byte xs)))

(deftest reads-plain-edn
  (is (= {:a [1 2 3] :b #{"x"} :c '(sym nil true false) :d \c :e 1.5}
         (edn/read-string "{:a [1 2 3] :b #{\"x\"} :c (sym nil true false) :d \\c :e 1.5}")))
  (is (= 42 (edn/read-string "  42  ")))
  (is (= "text" (edn/read-string "\"text\"")))
  (is (= :k (edn/read-string ";; a comment\n:k ; trailing\n")))
  (is (= [1 3] (edn/read-string "[1 #_ 2 3]")))
  (is (= [3] (edn/read-string "[#_ #_ 1 2 3]")))
  (is (= #:a{:b 1} (edn/read-string "#:a{:b 1}")))
  (is (= [1] (edn/read-string "^:m [1]")))
  (is (= ##Inf (edn/read-string "##Inf")))
  (is (= ::edn/malformed (refused "##")))
  (is (= 12345678901234567890N (edn/read-string "12345678901234567890N")))
  (is (= [\space \newline (char 0x5B)] (edn/read-string (str "[\\space \\newline \\u" "005B]")))))

(deftest every-entry-point-reads-the-same-text
  (let [text "{:a [1 {:b \"é\"}]}"
        expected {:a [1 {:b "é"}]}
        dir (fs/create-temp-dir {:prefix "briko-edn-"})]
    (try
      (let [file (fs/path dir "x.edn")
            bs (.getBytes text "UTF-8")]
        (fs/write-bytes file bs)
        (is (= expected (edn/read-string text)))
        (is (= expected (edn/read-bytes bs)))
        (is (= expected (edn/read-stream (ByteArrayInputStream. bs))))
        (is (= expected (edn/read-file file)))
        (is (= expected (edn/read-file (str file)))))
      (finally (fs/delete-tree dir)))))

(deftest options-are-checked
  (is (= ::edn/invalid-options (refused "1" {:max-byte 10})))
  (is (= ::edn/invalid-options (refused "1" {:reader {}})))
  (is (= ::edn/invalid-options (refused "1" [:max-bytes 10])))
  (is (= ::edn/invalid-options (refused "1" {:max-bytes 0})))
  (is (= ::edn/invalid-options (refused "1" {:max-bytes -1})))
  (is (= ::edn/invalid-options (refused "1" {:max-bytes Integer/MAX_VALUE})))
  (is (= ::edn/invalid-options (refused "1" {:max-bytes 1.5})))
  (is (= ::edn/invalid-options (refused "1" {:max-depth 0})))
  (is (= ::edn/invalid-options (refused "1" {:max-depth "64"})))
  (is (= ::edn/invalid-options (refused "1" {:max-number-chars 0})))
  (is (= ::edn/invalid-options (refused "1" {:readers {"inst" identity}})))
  (is (= ::edn/invalid-options (refused "1" {:readers {'inst "not a function"}})))
  (is (= ::edn/invalid-options (refused "1" {:readers nil})))
  (is (= ::edn/invalid-options (refused "1" {:one-form? nil})))
  (is (= ::edn/invalid-options (refused "1" {:expect "map?"})))
  (is (= 1 (edn/read-string "1" nil)))
  (is (= 1 (edn/read-string "1" {})))
  (is (= 1 (edn/read-string "1" edn/defaults)))
  (testing "every entry point checks its options first"
    (is (= ::edn/invalid-options (refusal #(edn/read-bytes (byte-array 0) {:bogus 1}))))
    (is (= ::edn/invalid-options (refusal #(edn/read-stream (ByteArrayInputStream. (byte-array 0)) {:bogus 1}))))
    (is (= ::edn/invalid-options (refusal #(edn/read-file "/nonexistent" {:bogus 1}))))))

(deftest inputs-of-the-wrong-type-are-refused
  (is (= ::edn/invalid-input (refused nil)))
  (is (= ::edn/invalid-input (refused 42)))
  (is (= ::edn/invalid-input (refused (.getBytes "1" "UTF-8"))))
  (is (= ::edn/invalid-input (refusal #(edn/read-bytes "1"))))
  (is (= ::edn/invalid-input (refusal #(edn/read-bytes nil))))
  (is (= ::edn/invalid-input (refusal #(edn/read-stream "1"))))
  (is (= ::edn/invalid-input (refusal #(edn/read-stream nil)))))

(deftest the-byte-limit-holds-at-its-boundary
  (let [limit 16
        at (str "\"" (str/join (repeat (- limit 2) "a")) "\"")
        over (str "\"" (str/join (repeat (dec limit) "a")) "\"")]
    (is (= limit (count at)))
    (testing "a string"
      (is (string? (edn/read-string at {:max-bytes limit})))
      (is (= ::edn/too-large (refused over {:max-bytes limit}))))
    (testing "bytes"
      (is (string? (edn/read-bytes (.getBytes at "UTF-8") {:max-bytes limit})))
      (is (= ::edn/too-large (refusal #(edn/read-bytes (.getBytes over "UTF-8") {:max-bytes limit})))))
    (testing "a stream"
      (is (string? (edn/read-stream (ByteArrayInputStream. (.getBytes at "UTF-8")) {:max-bytes limit})))
      (is (= ::edn/too-large (refusal #(edn/read-stream (ByteArrayInputStream. (.getBytes over "UTF-8"))
                                                        {:max-bytes limit})))))
    (testing "the refusal names the limit"
      (is (= limit (:limit (ex-data (try (edn/read-string over {:max-bytes limit})
                                         (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest the-byte-limit-counts-utf-8-bytes-not-characters
  ;; "é" is one character and two bytes; "😀" is two characters and four
  (is (= "éé" (edn/read-string "\"éé\"" {:max-bytes 6})))
  (is (= ::edn/too-large (refused "\"éé\"" {:max-bytes 5})))
  (is (= "😀" (edn/read-string "\"😀\"" {:max-bytes 6})))
  (is (= ::edn/too-large (refused "\"😀\"" {:max-bytes 5}))))

(deftest a-stream-is-read-no-further-than-one-byte-past-the-limit
  (let [total 10000
        in (ByteArrayInputStream. (byte-array total (byte (int \space))))]
    (is (= ::edn/too-large (refusal #(edn/read-stream in {:max-bytes 100}))))
    (is (= (- total 101) (.available in)))))

(deftest invalid-utf-8-is-refused
  (testing "a truncated sequence"
    (is (= ::edn/invalid-utf8 (refusal #(edn/read-bytes (bytes-of [0x22 0xC3 0x22]))))))
  (testing "an overlong encoding of NUL"
    (is (= ::edn/invalid-utf8 (refusal #(edn/read-bytes (bytes-of [0x22 0xC0 0x80 0x22]))))))
  (testing "an encoded surrogate"
    (is (= ::edn/invalid-utf8 (refusal #(edn/read-bytes (bytes-of [0x22 0xED 0xA0 0x80 0x22]))))))
  (testing "a lone continuation byte"
    (is (= ::edn/invalid-utf8 (refusal #(edn/read-stream (ByteArrayInputStream. (bytes-of [0x31 0x80]))))))
    (is (= ::edn/invalid-utf8 (refusal #(edn/read-bytes (bytes-of [0xFF]))))))
  (testing "a string holding an unpaired surrogate"
    (is (= ::edn/invalid-utf8 (refused (str "\"" (char 0xD800) "\""))))
    (is (= ::edn/invalid-utf8 (refused (str "\"" (char 0xDC00) "x\""))))))

(deftest the-depth-limit-holds-at-its-boundary
  (doseq [[open close] [["[" "]"] ["(" ")"] ["{:k " "}"] ["#{" "}"]]]
    (testing open
      (is (some? (edn/read-string (nest 8 open close "1") {:max-depth 8})))
      (is (= ::edn/too-deep (refused (nest 9 open close "1") {:max-depth 8}))))))

(deftest the-default-depth-limit-is-64
  (is (vector? (edn/read-string (nest 64 "[" "]" ""))))
  (is (= ::edn/too-deep (refused (nest 65 "[" "]" "")))))

(deftest a-map-counts-its-keys-and-values-one-level-down
  (is (= {[1] [2]} (edn/read-string "{[1] [2]}" {:max-depth 2})))
  (is (= ::edn/too-deep (refused "{[[1]] 2}" {:max-depth 2})))
  (is (= ::edn/too-deep (refused "{1 [[2]]}" {:max-depth 2}))))

(deftest siblings-do-not-add-up
  (is (= [[1] [2] [3]] (edn/read-string "[[1] [2] [3]]" {:max-depth 2})))
  (is (= [[[]] [[]]] (edn/read-string "[[[]] [[]]]" {:max-depth 3}))))

(deftest prefixes-the-reader-recurses-on-count-as-levels
  (let [readers {'t identity}]
    (testing "tagged literals"
      (is (= 1 (edn/read-string (str (str/join (repeat 8 "#t ")) "1") {:max-depth 8 :readers readers})))
      (is (= ::edn/too-deep (refused (str (str/join (repeat 9 "#t ")) "1") {:max-depth 8 :readers readers})))
      (is (= [[1]] (edn/read-string "#t [#t [1]]" {:max-depth 4 :readers readers})))
      (is (= ::edn/too-deep (refused "#t [#t [1]]" {:max-depth 3 :readers readers}))))
    (testing "a tagged literal completes and frees its level"
      (is (= [1 1 1] (edn/read-string "[#t 1 #t 1 #t 1]" {:max-depth 2 :readers readers}))))
    (testing "metadata"
      (is (= [1] (edn/read-string (str (str/join (repeat 7 "^:m ")) "[1]") {:max-depth 8})))
      (is (= ::edn/too-deep (refused (str (str/join (repeat 8 "^:m ")) "[1]") {:max-depth 8})))
      (is (= [1] (edn/read-string "#^:m [1]" {:max-depth 2})))
      (is (= ::edn/too-deep (refused "#^:m [1]" {:max-depth 1}))))
    (testing "metadata waits for its value after the metadata form"
      (is (= [[1] [2]] (edn/read-string "[^{:a 1} [1] ^{:b 2} [2]]" {:max-depth 3})))
      (is (= ::edn/too-deep (refused "[^{:a [1]} [1]]" {:max-depth 3}))))
    (testing "discards"
      (is (= [9] (edn/read-string (str "[" (str/join (repeat 7 "#_")) (str/join (repeat 7 "1 ")) "9]")
                                  {:max-depth 8})))
      (is (= ::edn/too-deep (refused (str "[" (str/join (repeat 8 "#_")) (str/join (repeat 8 "1 ")) "9]")
                                     {:max-depth 8}))))
    (testing "a discard yields no form, so the prefix around it keeps waiting"
      (is (= 2 (edn/read-string "#t #_ 1 2" {:max-depth 2 :readers readers})))
      (is (= [2 3] (edn/read-string "[#t #_ 1 2 3]" {:max-depth 3 :readers readers}))))
    (testing "namespaced maps"
      (is (= #:a{:b #:c{:d 1}} (edn/read-string "#:a{:b #:c{:d 1}}" {:max-depth 4})))
      (is (= ::edn/too-deep (refused "#:a{:b #:c{:d 1}}" {:max-depth 3})))
      (is (= {} (edn/read-string "#: a{}" {:max-depth 2}))))
    (testing "symbolic values"
      (is (= ##Inf (edn/read-string "##Inf" {:max-depth 1})))
      (is (= ##-Inf (edn/read-string "## -Inf" {:max-depth 1})))
      (is (= [##Inf] (edn/read-string "[##Inf]" {:max-depth 2})))
      (is (= ::edn/too-deep (refused "[##Inf]" {:max-depth 1})))
      (is (= [##Inf ##-Inf] (edn/read-string "[##Inf ##-Inf]" {:max-depth 2})))
      (is (= ::edn/malformed (refused (str (str/join (repeat 8 "## ")) "Inf") {:max-depth 8})))
      (is (= ::edn/too-deep (refused (str (str/join (repeat 9 "## ")) "Inf") {:max-depth 8}))))
    (testing "symbolic values mixed with other prefixes"
      (is (= ##Inf (edn/read-string "#t ##Inf" {:max-depth 2 :readers readers})))
      (is (= ::edn/too-deep (refused "#t ##Inf" {:max-depth 1 :readers readers})))
      (is (= [1] (edn/read-string "[#_ ##Inf 1]" {:max-depth 3})))
      (is (= ::edn/too-deep (refused "[#_ ##Inf 1]" {:max-depth 2})))
      (is (= ::edn/malformed (refused "#t ## #t ## [##Inf]" {:max-depth 6 :readers readers})))
      (is (= ::edn/too-deep (refused "#t ## #t ## [##Inf]" {:max-depth 5 :readers readers})))
      (is (= ::edn/malformed (refused "^:m ## ^:m ## [1]" {:max-depth 5})))
      (is (= ::edn/too-deep (refused "^:m ## ^:m ## [1]" {:max-depth 4})))
      (is (= ::edn/malformed (refused "#:a{:b ## #:c{:d ##Inf}}" {:max-depth 6})))
      (is (= ::edn/too-deep (refused "#:a{:b ## #:c{:d ##Inf}}" {:max-depth 5}))))))

(defn- scan-refusal
  "The :type the depth scan alone refuses s with, or :no-refusal, so a
   test can tell the scan from the reader's StackOverflowError backstop."
  [s opts]
  (refusal #(#'edn/scan! s (merge edn/defaults opts))))

(deftest the-scan-alone-refuses-symbolic-value-chains
  (doseq [text [(str (str/join (repeat 200000 "## ")) "Inf")
                (str (str/join (repeat 200000 "##")) "Inf")
                (str (str/join (repeat 100000 "#t ## ")) "Inf")
                (str (str/join (repeat 100000 "## [")) "1")]]
    (is (= ::edn/too-deep (scan-refusal text {:max-depth 100000 :readers {'t identity}}))
        (subs text 0 8))
    (is (= ::edn/too-deep (scan-refusal text {:readers {'t identity}}))
        (subs text 0 8))
    (is (= ::edn/too-deep (refused text {:max-bytes 2000000 :readers {'t identity}}))
        (subs text 0 8)))
  (is (= ::edn/too-deep (scan-refusal (str (str/join (repeat 65 "## ")) "Inf") {})))
  (is (= :no-refusal (scan-refusal (str (str/join (repeat 64 "## ")) "Inf") {}))))

(deftest deep-input-is-refused-before-the-reader-overflows
  ;; clojure.edn throws StackOverflowError on this input on the JVM and
  ;; under babashka; the scan refuses it as too deep instead.
  (doseq [text [(str/join (repeat 200000 "["))
                (nest 200000 "[" "]" "")
                (str (str/join (repeat 200000 "#t ")) "1")
                (str (str/join (repeat 200000 "^:a ")) "[]")
                (str (str/join (repeat 200000 "#_")) (str/join (repeat 200001 "1 ")))]]
    (is (= ::edn/too-deep (refused text {:max-depth 1000000 :max-bytes 2000000 :readers {'t identity}}))
        (subs text 0 8))
    (is (= ::edn/too-deep (refused text {:max-bytes 2000000 :readers {'t identity}}))
        (subs text 0 8))))

(deftest brackets-inside-strings-comments-and-characters-do-not-count
  (let [opts {:max-depth 1}]
    (is (= ["[[[[" "]]]]"] (edn/read-string "[\"[[[[\" \"]]]]\"]" opts)))
    (is (= ["{(#{"] (edn/read-string "[\"{(#{\"]" opts)))
    (is (= [1] (edn/read-string "[1 ; [[[[ {{{{ ((((\n]" opts)))
    (is (= [1] (edn/read-string "[1 ; [[[[\r]" opts)))
    (is (= [\[ \[ \( \{] (edn/read-string "[\\[ \\[ \\( \\{]" opts)))
    (is (= [\] \)] (edn/read-string "[\\] \\)]" opts)))
    (testing "an escaped quote does not end a string"
      (is (= ["\"[[[" 1] (edn/read-string "[\"\\\"[[[\" 1]" opts))))
    (testing "an escaped backslash does end a string"
      (is (= ["\\" 1] (edn/read-string "[\"\\\\\" 1]" opts)))
      (is (= ::edn/too-deep (refused "[\"\\\\\" [1]]" opts))))
    (testing "a quote character is not a string"
      (is (= [\" 1] (edn/read-string "[\\\" 1]" opts)))
      (is (= ::edn/too-deep (refused "[\\\" [1]]" opts))))
    (testing "a semicolon character is not a comment"
      (is (= [\; 1] (edn/read-string "[\\; 1]" opts)))
      (is (= ::edn/too-deep (refused "[\\; [1]]" opts))))
    (testing "a comment character inside a string is not a comment"
      (is (= ::edn/too-deep (refused "[\";\" [1]]" opts))))
    (testing "a quote inside a comment opens no string"
      (is (= ::edn/too-deep (refused "[; \"\n[1] \"]" opts))))
    (testing "a symbolic value inside a string, comment or character does not count"
      (is (= ["## ## ##"] (edn/read-string "[\"## ## ##\"]" opts)))
      (is (= [1] (edn/read-string "[1 ; ## ## ## ##\n]" opts)))
      (is (= [\# \# 1] (edn/read-string "[\\# \\# 1]" opts))))))

(deftest unmatched-closers-do-not-lower-the-count
  (is (= ::edn/malformed (refused (str "]]]]]]" (nest 3 "[" "]" "")) {:max-depth 3})))
  (is (= ::edn/too-deep (refused (str "]]]]]]" (nest 4 "[" "]" "")) {:max-depth 3})))
  (is (= ::edn/too-deep (refused (str "[1] ]]]] " (nest 4 "[" "]" "")) {:max-depth 3 :one-form? false}))))

(deftest tagged-literals-are-refused-unless-a-reader-is-given
  (is (= ::edn/tag-refused (refused "#inst \"2026-11-27T00:00:00Z\"")))
  (is (= ::edn/tag-refused (refused "#uuid \"00000000-0000-0000-0000-000000000000\"")))
  (is (= ::edn/tag-refused (refused "#foo 1")))
  (is (= ::edn/tag-refused (refused "#my.ns/foo [1]")))
  (is (= ::edn/tag-refused (refused "[1 #_ #foo 2 3]")))
  (is (= ::edn/tag-refused (refused "#foo#bar 1" {:readers {'foo identity}})))
  (is (= 'foo (:tag (ex-data (try (edn/read-string "{:a #foo 1}") (catch clojure.lang.ExceptionInfo e e))))))
  (testing "a tag-like text inside a string or comment is no tag"
    (is (= "#foo 1" (edn/read-string "\"#foo 1\"")))
    (is (= 1 (edn/read-string "; #foo\n1")))
    (is (= 'a#foo (edn/read-string "a#foo")))))

(deftest tagged-literals-read-with-the-readers-given
  (let [readers {'inst #(Instant/parse %) 'uuid #(UUID/fromString %) 'my/pair (fn [[a b]] {:a a :b b})}]
    (is (= (Instant/parse "2026-11-27T00:00:00Z")
           (edn/read-string "#inst \"2026-11-27T00:00:00Z\"" {:readers readers})))
    (is (= (UUID/fromString "00000000-0000-0000-0000-000000000001")
           (edn/read-string "#uuid \"00000000-0000-0000-0000-000000000001\"" {:readers readers})))
    (is (= {:a 1 :b 2} (edn/read-string "#my/pair [1 2]" {:readers readers})))
    (testing "a reader that throws is a tag failure naming the tag"
      (let [e (try (edn/read-string "#inst \"yesterday\"" {:readers readers})
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= ::edn/tag-failed (:type (ex-data e))))
        (is (= 'inst (:tag (ex-data e))))))))

(deftest long-number-literals-are-refused
  (let [digits (fn [n] (str/join (repeat n "7")))]
    (is (integer? (edn/read-string (digits 100))))
    (is (= ::edn/number-too-long (refused (digits 101))))
    (is (= ::edn/number-too-long (refused (str "-" (digits 100)))))
    (is (= ::edn/number-too-long (refused (str "[1 " (digits 100) "N]"))))
    (is (vector? (edn/read-string (str "[1 " (digits 99) "N]"))))
    (is (= ::edn/number-too-long (refused (str (digits 60) "." (digits 60)))))
    (is (= ::edn/number-too-long (refused (str "#:a{:k " (digits 101) "}"))))
    (is (integer? (edn/read-string (digits 101) {:max-number-chars 101})))
    (is (= ::edn/number-too-long (refused "123456" {:max-number-chars 5})))
    (is (= 100 (:limit (ex-data (try (edn/read-string (digits 101)) (catch clojure.lang.ExceptionInfo e e)))))))
  (testing "a long symbol or string is no number"
    (is (symbol? (edn/read-string (str "a" (str/join (repeat 500 "1"))))))
    (is (symbol? (edn/read-string (str "-a" (str/join (repeat 500 "1"))))))
    (is (string? (edn/read-string (str "\"" (str/join (repeat 500 "1")) "\""))))))

(deftest exactly-one-form-is-read-by-default
  (is (= ::edn/no-form (refused "")))
  (is (= ::edn/no-form (refused "   \n\t,")))
  (is (= ::edn/no-form (refused ";; only a comment")))
  (is (= ::edn/no-form (refused "#_ 1")))
  (is (= ::edn/trailing-form (refused "1 2")))
  (is (= ::edn/trailing-form (refused "{:a 1} {:b 2}")))
  (is (= ::edn/trailing-form (refused "{:a 1}\n:junk")))
  (is (= {:a 1} (edn/read-string "{:a 1} #_ :ignored ; and a comment"))))

(deftest every-form-is-read-when-asked
  (is (= [] (edn/read-string "" {:one-form? false})))
  (is (= [1 {:a 2} [3]] (edn/read-string "1 {:a 2} [3]" {:one-form? false})))
  (is (= [1 3] (edn/read-string "1 #_ 2 3" {:one-form? false})))
  (is (= ::edn/malformed (refused "1 {:a" {:one-form? false}))))

(deftest forms-must-satisfy-expect
  (is (= {:a 1} (edn/read-string "{:a 1}" {:expect map?})))
  (let [e (try (edn/read-string "[1]" {:expect map?}) (catch clojure.lang.ExceptionInfo e e))]
    (is (= ::edn/unexpected (:type (ex-data e))))
    (is (= "a vector" (:found (ex-data e)))))
  (is (= ::edn/unexpected (refused "nil" {:expect some?})))
  (is (= [{:a 1} {:b 2}] (edn/read-string "{:a 1} {:b 2}" {:one-form? false :expect map?})))
  (is (= ::edn/unexpected (refused "{:a 1} 2" {:one-form? false :expect map?}))))

(deftest anything-beyond-edn-is-malformed
  (doseq [text ["{:a" "]" ")" "[1 2" "\"unterminated" "#=(+ 1 2)" "#\"regex\"" "#(inc %)"
                "#?(:clj 1)" "#'var" "`a" "@a" "~a" "{:a 1 :a 2}" "#{1 1}" "{:a}" "#<x>" "# a" "#1"
                "\\abc" "1#a" "#:a 1"]]
    (is (= ::edn/malformed (refused text)) text)))

(deftest read-file-refuses-what-is-not-a-readable-regular-file
  (let [dir (fs/create-temp-dir {:prefix "briko-edn-"})]
    (try
      (let [file (str (fs/path dir "x.edn"))]
        (testing "a directory"
          (is (= ::edn/not-a-file (refusal #(edn/read-file dir)))))
        (testing "a missing file"
          (is (= ::edn/not-a-file (refusal #(edn/read-file (fs/path dir "missing.edn"))))))
        (testing "a file too large"
          (spit file "[1 2 3]")
          (is (= ::edn/too-large (refusal #(edn/read-file file {:max-bytes 6}))))
          (is (= [1 2 3] (edn/read-file file {:max-bytes 7}))))
        (testing "every refusal names the file"
          (spit file "[1 2")
          (let [e (try (edn/read-file file) (catch clojure.lang.ExceptionInfo e e))]
            (is (= ::edn/malformed (:type (ex-data e))))
            (is (= file (:file (ex-data e))))
            (is (str/starts-with? (ex-message e) file))))
        (testing "a file nobody may read"
          (spit file "1")
          (fs/set-posix-file-permissions file "---------")
          ;; root reads the file anyway, so the refusal shows only for others
          (when-not (fs/readable? file)
            (is (= ::edn/unreadable (refusal #(edn/read-file file)))))))
      (finally (fs/delete-tree dir)))))

(deftest every-refusal-type-is-listed
  (is (every? #(= "briko.edn" (namespace %)) edn/refusal-types))
  (is (not (edn/refusal? (ex-info "foreign" {:type :other}))))
  (is (not (edn/refusal? (RuntimeException. "plain")))))
