;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.lazy-fixture
  "A namespace briko.json-test loads through briko.json's var loader after
   interning one of its vars unbound, as a data_readers.clj entry does.")

(defn answer [] 42)
