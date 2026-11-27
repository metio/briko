;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.edn-property-test
  (:require [briko.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

;; babashka carries test.check without its clojure.test integration, so
;; each property runs through quick-check inside a deftest.
(defn- holds?
  [num-tests property]
  (let [result (tc/quick-check num-tests property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :fail :shrunk])))))

;; Data that prints as EDN and reads back equal: NaN is left out because
;; it equals nothing, itself included.
(def ^:private scalar
  (gen/one-of [gen/large-integer
               (gen/double* {:NaN? false})
               gen/string
               gen/keyword
               gen/keyword-ns
               gen/symbol
               gen/symbol-ns
               gen/boolean
               gen/char
               (gen/return nil)]))

(def ^:private data
  (gen/recursive-gen (fn [inner]
                       (gen/one-of [(gen/vector inner)
                                    (gen/list inner)
                                    (gen/set inner)
                                    (gen/map inner inner)]))
                     scalar))

(defn- print-plain
  "x printed as EDN without namespaced-map syntax, which counts a level
   of its own."
  [x]
  (binding [*print-namespace-maps* false]
    (pr-str x)))

(defn- depth
  "How deep x nests collections, a map's keys and values one level below
   the map. An infinite double prints as ##Inf or ##-Inf, whose ## the
   reader recurses through, so it counts one level."
  [x]
  (cond
    (coll? x) (inc (reduce max 0 (map depth (if (map? x) (mapcat identity x) x))))
    (and (double? x) (Double/isInfinite x)) 1
    :else 0))

(deftest printed-data-reads-back-equal
  (holds? 300
          (prop/for-all [x data]
            (= x (edn/read-string (print-plain x) {:max-depth 1000 :max-number-chars 1000})))))

(deftest data-reads-exactly-when-it-nests-within-the-limit
  (holds? 300
          (prop/for-all [x data
                         limit (gen/choose 1 6)]
            (let [result (try
                           (edn/read-string (print-plain x) {:max-depth limit :max-number-chars 1000})
                           (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
              (if (<= (depth x) limit)
                (= x result)
                (= ::edn/too-deep result))))))

;; Text built from prefixes as well as collections, with the number of
;; levels clojure.edn recurses through to read it: one per collection, per
;; tagged literal, per symbolic value, per discard and per metadata prefix,
;; and two per namespaced map (the prefix and its map). The property checks the scan
;; against this model of the reader.
(def ^:private syntax
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of
      [(gen/fmap (fn [xs] [(str "[" (str/join " " (map first xs)) "]")
                           (inc (reduce max 0 (map second xs)))])
                 (gen/vector inner 0 4))
       (gen/fmap (fn [xs] [(str "(" (str/join " " (map first xs)) ")")
                           (inc (reduce max 0 (map second xs)))])
                 (gen/vector inner 0 4))
       (gen/fmap (fn [[t d]] [(str "#t " t) (inc d)]) inner)
       ;; metadata needs a value that takes it, so it wraps a vector
       (gen/fmap (fn [[t d]] [(str "^:m [" t "]") (+ 2 d)]) inner)
       (gen/fmap (fn [[[t d] [u e]]] [(str "[#_ " t " " u "]") (inc (max (inc d) e))])
                 (gen/tuple inner inner))
       (gen/fmap (fn [xs] [(str "#:ns{" (str/join " " (map-indexed (fn [i [t _]] (str ":k" i " " t)) xs)) "}")
                           (+ 2 (reduce max 0 (map second xs)))])
                 (gen/vector inner 0 3))]))
   (gen/elements [["1" 0] [":k" 0] ["\"[{(\"" 0] ["\\[" 0] ["sym" 0] ["nil" 0]
                  ["##Inf" 1] ["## -Inf" 1] ["\"##\"" 0] ["\\#" 0]])))

(deftest text-reads-exactly-when-its-levels-are-within-the-limit
  (holds? 500
          (prop/for-all [[text levels] syntax
                         limit (gen/choose 1 8)]
            (let [result (try
                           (edn/read-string text {:max-depth limit :readers {'t identity}})
                           :read
                           (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
              (= result (if (<= levels limit) :read ::edn/too-deep))))))

(deftest symbolic-value-chains-are-refused-as-too-deep-exactly-beyond-the-limit
  ;; ## reads the form after it, so k of them before any text add k
  ;; levels; the reader then refuses every such chain, because the form
  ;; after ## is not one of the symbols Inf, -Inf or NaN.
  (holds? 500
          (prop/for-all [[text levels] syntax
                         k (gen/choose 1 6)
                         limit (gen/choose 1 10)]
            (let [result (try
                           (edn/read-string (str (str/join (repeat k "## ")) text)
                                            {:max-depth limit :readers {'t identity}})
                           :read
                           (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
              (= result (if (<= (+ k levels) limit) ::edn/malformed ::edn/too-deep))))))

(defn- outcome
  "What reading does: :read, the :type of a refusal, or the class of
   anything else thrown, which the properties below forbid."
  [f]
  (try
    (f)
    :read
    (catch clojure.lang.ExceptionInfo e
      (if (edn/refusal? e) :refused [:escaped (ex-message e)]))
    ;; A StackOverflowError is exactly what must not escape, so the
    ;; property catches Errors too.
    #_{:splint/disable [lint/catch-throwable]}
    (catch Throwable t
      [:escaped (.getName (class t))])))

(deftest random-bytes-read-or-are-refused
  (holds? 500
          (prop/for-all [bs gen/bytes]
            (contains? #{:read :refused} (outcome #(edn/read-bytes bs {:max-depth 8 :max-bytes 4096}))))))

;; Strings dense in the characters the scan and the reader treat
;; specially, so they hit their corners far more often than random bytes.
(def ^:private edn-ish
  (gen/fmap str/join
            (gen/vector (gen/elements ["[" "]" "{" "}" "(" ")" "#" "_" "^" ":" "\"" "\\" ";" "\n" " " ","
                                       "a" "1" "-" "+" "." "N" "M" "/" "#t " "#inst " "#_ " "^:m " "#:a" "##"])
                        0 200)))

(deftest syntax-dense-strings-read-or-are-refused
  (holds? 1000
          (prop/for-all [s edn-ish
                         one-form? gen/boolean]
            (contains? #{:read :refused}
                       (outcome #(edn/read-string s {:max-depth 4 :one-form? one-form? :readers {'t identity}}))))))
