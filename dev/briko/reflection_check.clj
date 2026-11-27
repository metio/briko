;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.reflection-check
  "Loads every namespace under a source directory with reflection warnings
   on and exits non-zero when the compiler printed a warning. briko runs
   in GraalVM native images, where a reflective call fails at run time."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File StringWriter)))

(defn- namespaces
  [^String root]
  (->> (file-seq (io/file root))
       (filter (fn [^File f] (and (.isFile f) (str/ends-with? (.getName f) ".clj"))))
       (map (fn [^File f]
              (-> (subs (.getPath f) (inc (count root)))
                  (str/replace #"\.clj$" "")
                  (str/replace File/separator ".")
                  (str/replace "_" "-")
                  symbol)))
       sort))

(defn -main
  [root]
  (let [nss (namespaces root)
        err (StringWriter.)]
    (binding [*err* err
              *warn-on-reflection* true]
      (doseq [ns nss] (require ns)))
    ;; The compiler prints reflection and primitive-loop boxing warnings
    ;; to *err*; loading prints nothing else, so any output fails the check.
    (let [warnings (remove str/blank? (str/split-lines (str err)))]
      (println "loaded" (count nss) "namespaces")
      (when (empty? nss)
        (println "no namespace found under" root))
      (doseq [w warnings] (println w))
      (shutdown-agents)
      (System/exit (if (or (empty? nss) (seq warnings)) 1 0)))))
