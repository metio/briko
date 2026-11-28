;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json-bench
  "Measures briko.json against cheshire on generated documents shaped
   like kubectl, npm and GitHub output, and write-string on a map shaped
   like tik's derivation cache. Prints one EDN map per case. Run it on
   both runtimes:

     bb -cp src:test:dev -m briko.json-bench 50
     clojure -M:dev -m briko.json-bench 50"
  (:require [briko.json :as json]
            [briko.json-fixtures :as fixtures]
            [cheshire.core :as cheshire]))

(set! *warn-on-reflection* true)

(defn- ms-per-run
  "Calls f n times after a warm-up and returns {:median-ms :mean-ms :p90-ms}."
  [n f]
  (dotimes [_ (max 3 (quot n 5))] (f))
  (let [times (vec (sort (for [_ (range n)]
                           (let [start (System/nanoTime)]
                             (f)
                             (/ (- (System/nanoTime) start) 1e6)))))
        at (fn [q] (nth times (min (dec n) (long (* q n)))))
        round (fn [x] (/ (Math/round (* 1000.0 (double x))) 1000.0))]
    {:median-ms (round (at 0.5))
     :p90-ms (round (at 0.9))
     :mean-ms (round (/ (reduce + times) n))}))

(defn- realise
  "cheshire's parse with every lazy part forced, as briko.json returns it."
  [s]
  (doall (cheshire/parse-string s)))

(defn- realise-strict
  "The same with briko.json's factory, duplicate detection and limits
   included, so the difference to briko.json is its own checks."
  [factory s]
  (with-bindings {(requiring-resolve 'cheshire.factory/*json-factory*) factory}
    (doall (cheshire/parse-string s))))

(defn -main
  [& [n]]
  (let [n (or (some-> n parse-long) 50)
        runtime (if (System/getProperty "babashka.version") :bb :jvm)
        docs [[:kubectl-pods (cheshire/generate-string (fixtures/pod-list 1 60))]
              [:npm-document (cheshire/generate-string (fixtures/npm-document 2 300))]
              [:github-releases (cheshire/generate-string (fixtures/release-list 3 100))]]
        cache (fixtures/derivation-cache 4 2200)
        opts {:max-bytes 16777216}
        base {:runtime runtime :runs n
              :java (System/getProperty "java.version")
              :babashka (System/getProperty "babashka.version")}]
    (doseq [[doc text] docs]
      (let [factory (#'json/factory (merge json/defaults opts))
            c (ms-per-run n #(realise text))
            strict (ms-per-run n #(realise-strict factory text))
            b (ms-per-run n #(json/parse-string text opts))]
        (prn (merge base {:case :parse :doc doc :chars (count text)
                          :cheshire-median-ms (:median-ms c) :cheshire-strict-factory-median-ms (:median-ms strict)
                          :briko-median-ms (:median-ms b)
                          :ratio (/ (Math/round (* 100.0 (/ (:median-ms b) (max 0.001 (:median-ms c))))) 100.0)}))))
    (let [text (cheshire/generate-string cache)
          c (ms-per-run n #(cheshire/generate-string cache))
          b (ms-per-run n #(json/write-string cache))
          w (ms-per-run n #(json/write-string cache {:allow-duplicate-keys? true}))
          e (ms-per-run n #(json/escape-hidden text))]
      (prn (merge base {:case :write :doc :derivation-cache :chars (count text) :keys (count cache)
                        :cheshire-median-ms (:median-ms c) :briko-median-ms (:median-ms b)
                        :briko-no-duplicate-walk-median-ms (:median-ms w)
                        :escape-hidden-median-ms (:median-ms e)
                        :ratio (/ (Math/round (* 100.0 (/ (:median-ms b) (max 0.001 (:median-ms c))))) 100.0)})))
    (shutdown-agents)))
