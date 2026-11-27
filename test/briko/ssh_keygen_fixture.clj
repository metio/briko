;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.ssh-keygen-fixture
  "Helpers for the tests that run the real ssh-keygen. Those tests fail
   when ssh-keygen is not on PATH, because a skipped oracle must never
   read as a passed one; BRIKO_SKIP_SSH_KEYGEN=true skips them instead,
   and each skip prints a line naming the test."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def available?
  (delay (try
           (process/shell {:continue true :out :string :err :string :in ""}
                          "ssh-keygen" "-Y" "check-novalidate" "-n" "x" "-s" "/nonexistent")
           true
           (catch Exception _ false))))

(defn ssh-keygen?
  "Whether the calling test may run ssh-keygen. Fails the test when
   ssh-keygen is missing, unless BRIKO_SKIP_SSH_KEYGEN=true asks to skip,
   in which case it prints the skip and answers false."
  [test-name]
  (cond
    @available? true
    (= "true" (System/getenv "BRIKO_SKIP_SSH_KEYGEN"))
    (do (println "SKIPPED" test-name "- ssh-keygen is not on PATH and BRIKO_SKIP_SSH_KEYGEN=true")
        false)
    :else
    (do (is false (str test-name " needs ssh-keygen on PATH; set BRIKO_SKIP_SSH_KEYGEN=true to skip it"))
        false)))

(defn keygen!
  "A fresh key pair of `type` named `who` in dir: {:key path :public line}."
  [dir who type]
  (let [key (str (fs/path dir who))]
    (process/shell {:out :string :err :string :in ""}
                   "ssh-keygen" "-q" "-t" type "-N" "" "-C" who "-f" key)
    {:key key
     :public (str/trim (slurp (str key ".pub")))}))

(defn keygen-verifies?
  "ssh-keygen's own verdict on the armored signature over message."
  [dir allowed principal namespace ^bytes message armor]
  (let [a (fs/create-temp-file {:dir dir :prefix "allowed"})
        s (fs/create-temp-file {:dir dir :prefix "sig"})]
    (spit (str a) (str allowed "\n"))
    (spit (str s) armor)
    (zero? (:exit (process/shell {:continue true :in (java.io.ByteArrayInputStream. message) :out :string :err :string}
                                 "ssh-keygen" "-Y" "verify" "-f" (str a) "-I" principal
                                 "-n" namespace "-s" (str s))))))
