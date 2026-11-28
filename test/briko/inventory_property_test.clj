;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.inventory-property-test
  (:require [briko.inventory :as inventory]
            [clojure.edn :as edn]
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

(def ^:private non-blank
  (gen/such-that (complement str/blank?) gen/string 100))

(def ^:private strings (gen/vector gen/string 0 4))

(def ^:private properties
  (gen/map gen/keyword (gen/one-of [gen/string strings]) {:max-elements 12}))

(def ^:private record
  (gen/let [required (gen/hash-map :host non-blank
                                   :path non-blank
                                   :name gen/string
                                   :namespace gen/string
                                   :default-branch (gen/one-of [gen/string (gen/return nil)])
                                   :archived gen/boolean
                                   :fork gen/boolean
                                   :topics strings)
            props (gen/one-of [(gen/return nil) properties])]
    (cond-> required props (assoc :properties props))))

(def ^:private sources
  (gen/map non-blank (gen/vector-distinct-by inventory/id record {:max-elements 6}) {:max-elements 4}))

(def ^:private header
  (gen/fmap (partial str/join "\n")
            (gen/vector (gen/one-of [(gen/return "")
                                     (gen/fmap #(str ";" (str/replace % #"[\r\n]" "")) gen/string)])
                        0 4)))

(defn- sorted-sources
  [sources]
  (update-vals sources #(vec (sort-by :path %))))

(deftest render-then-parse-gives-back-the-records
  (holds? 300 (prop/for-all [s sources h header]
                (let [text (inventory/render {:header h :sources s})]
                  (= (sorted-sources s) (:sources (inventory/parse text)))))))

(deftest render-of-parse-gives-back-the-text
  (holds? 300 (prop/for-all [s sources h header]
                (let [text (inventory/render {:header h :sources s})]
                  (= text (inventory/render (inventory/parse text)))))))

(deftest clojure-edn-reads-what-render-writes
  (holds? 300 (prop/for-all [s sources h header]
                (= (sorted-sources s) (edn/read-string (inventory/render {:header h :sources s}))))))

(deftest every-record-takes-one-line
  ;; a source takes the line of its id and one per record, or one for "[]"
  (holds? 300 (prop/for-all [s sources]
                (= (if (seq s) (reduce + (for [[_ records] s] (inc (max 1 (count records))))) 1)
                   (count (str/split-lines (inventory/render {:header "" :sources s})))))))
