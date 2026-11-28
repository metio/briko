;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.process-bench
  "Measures what a short run costs through briko.process against
   babashka.process, and how much of it the named pipes take. Prints one
   EDN map per case. Run it on both runtimes:

     bb -cp src:dev -m briko.process-bench 300
     clojure -M:dev -m briko.process-bench 300"
  (:require [babashka.fs :as fs]
            [babashka.process :as bp]
            [briko.process :as process]))

(set! *warn-on-reflection* true)

(defn- ms-per-run
  "Calls f n times after a warm-up and returns {:median-ms :mean-ms :p90-ms}."
  [n f]
  (dotimes [_ 20] (f))
  (let [times (vec (sort (for [_ (range n)]
                           (let [start (System/nanoTime)]
                             (f)
                             (/ (- (System/nanoTime) start) 1e6)))))
        at (fn [q] (nth times (min (dec n) (long (* q n)))))
        round (fn [x] (/ (Math/round (* 1000.0 (double x))) 1000.0))]
    {:median-ms (round (at 0.5))
     :p90-ms (round (at 0.9))
     :mean-ms (round (/ (reduce + times) n))}))

(defn- fifo-setup
  "What briko.process does for the pipes alone: a private directory, one
   mkfifo call for both pipes, and the removal."
  []
  (let [make-fifos! @#'briko.process/make-fifos!
        dir (str (fs/create-temp-dir {:prefix "briko-bench-"}))]
    (try
      (make-fifos! [(str dir "/out") (str dir "/err")] (+ (System/nanoTime) 60000000000))
      (finally (fs/delete-tree dir)))))

(defn -main
  [& [n]]
  (let [n (or (some-> n parse-long) 300)
        runtime (if (System/getProperty "babashka.version") :bb :jvm)
        cases [[:briko-git-version #(process/run ["git" "--version"])]
               [:babashka-process-git-version #(bp/shell {:out :string :err :string :continue true} "git" "--version")]
               [:briko-true #(process/run ["true"])]
               [:babashka-process-true #(bp/shell {:out :string :err :string :continue true} "true")]
               [:fifo-setup-only fifo-setup]]]
    (doseq [[case f] cases]
      (prn (merge {:runtime runtime :case case :runs n
                   :java (System/getProperty "java.version")
                   :babashka (System/getProperty "babashka.version")}
                  (ms-per-run n f))))
    (shutdown-agents)))
