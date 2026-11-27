;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.test-runner
  "Runs the test suite under babashka, where kaocha does not run. Every
   `*_test.clj` under the test directory is a test namespace, so a new one
   needs no registration. A run that finds no test namespace fails, so a
   misconfigured path cannot pass as a green run."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn- namespace-of
  [root file]
  (-> (str (fs/relativize root file))
      (str/replace #"\.clj$" "")
      (str/replace "/" ".")
      (str/replace "_" "-")
      symbol))

(defn run
  "Requires and runs the test namespaces under root, or only those named,
   and exits non-zero when a test fails or errors or none was found."
  [root names]
  (let [found (sort (map (partial namespace-of root) (fs/glob root "**_test.clj")))
        nss (if (seq names) (map symbol names) found)]
    (when (empty? nss)
      (binding [*out* *err*]
        (println "no test namespace found under" root))
      (System/exit 1))
    (apply require nss)
    (let [{:keys [test fail error]} (apply t/run-tests nss)]
      (when (zero? test)
        (binding [*out* *err*]
          (println "the test namespaces hold no test")))
      (System/exit (if (and (pos? test) (zero? (+ fail error))) 0 1)))))
