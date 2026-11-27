;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.deps-test
  "briko's namespaces build on babashka.fs and babashka.process. babashka
   carries both, and deps.edn declares them for the JVM, so they load in
   every runtime briko supports."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.test :refer [deftest is]]))

(deftest the-declared-dependencies-load
  (is (fs/directory? (fs/temp-dir)))
  (is (fn? process/process)))
