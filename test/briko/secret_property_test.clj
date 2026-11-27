;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.secret-property-test
  "Every secret in these tests is a generated dummy value."
  (:require [briko.process :as process]
            [briko.secret :as secret]
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

(def ^:private dummy-secret
  "Long enough that it cannot occur by chance in a message."
  (gen/fmap (fn [cs] (str "dummy-" (str/join cs)))
            (gen/vector gen/char-alphanumeric 12 24)))

(deftest first-line-is-never-blank
  (holds? 500
          (prop/for-all [s gen/string]
            (let [line (secret/first-line s)]
              (or (nil? line) (and (not (str/blank? line)) (= line (str/trim line))))))))

(deftest first-line-of-one-line-is-the-trimmed-line
  (holds? 300
          (prop/for-all [s (gen/such-that #(not (str/blank? %)) gen/string-alphanumeric)
                         pad (gen/elements ["" " " "\t" "  "])]
            (= s (secret/first-line (str pad s pad "\nlogin: x\n"))))))

(deftest field-in-finds-a-generated-field
  (holds? 300
          (prop/for-all [k (gen/such-that #(not (str/blank? %)) gen/string-alphanumeric)
                         v (gen/such-that #(not (str/blank? %)) gen/string-alphanumeric)]
            (= v (secret/field-in (str "password\n" k ":  " v "  \nother: y") k)))))

(deftest env-values-resolve-exactly-unless-blank
  (holds? 300
          (prop/for-all [v gen/string]
            (let [opts {:getenv {"TOKEN" v}}]
              (if (str/blank? v)
                (= ::secret/empty
                   (try (secret/resolve1 "token" {:env "TOKEN"} opts)
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
                (= v (secret/resolve1 "token" {:env "TOKEN"} opts)))))))

(deftest a-failing-command-never-leaks-its-output
  (holds? 25
          (prop/for-all [s dummy-secret
                         exit (gen/choose 1 125)]
            (let [spec {:command ["sh" "-c" (str "printf '%s\\n' \"$1\"; exit " exit) "sh" s]}]
              (try
                (secret/resolve1 "token" spec)
                false
                (catch clojure.lang.ExceptionInfo e
                  (and (= ::secret/command-failed (:type (ex-data e)))
                       (= exit (:exit (ex-data e)))
                       (not (str/includes? (str (ex-message e) (pr-str (ex-data e))) s)))))))))

(deftest a-command-yields-its-first-line
  (holds? 25
          (prop/for-all [s dummy-secret
                         rest-lines (gen/vector gen/string-alphanumeric 0 3)]
            (= s (secret/resolve1 "token" {:command (into ["printf" "%s\\n" s] rest-lines)})))))

(deftest stdin-comes-back-through-cat-unchanged
  (holds? 25
          (prop/for-all [s gen/string]
            (= s (:out (process/run ["cat"] {:in s}))))))

(deftest output-is-cut-exactly-at-the-cap
  (holds? 25
          (prop/for-all [n (gen/choose 0 70000)
                         cap (gen/choose 0 70000)]
            (let [r (process/run ["head" "-c" (str n) "/dev/zero"] {:max-out-bytes cap})]
              (and (zero? (:exit r))
                   (= (min n cap) (count (:out r)))
                   (= (> n cap) (:out-truncated? r)))))))
