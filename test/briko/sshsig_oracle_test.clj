;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.sshsig-oracle-test
  "briko.sshsig checked against the tool whose answers it reproduces: for
   the same bytes, the same verdict as ssh-keygen, and where they could
   differ, this reader refuses. Needs ssh-keygen; see
   briko.ssh-keygen-fixture for how a missing one is reported."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [briko.sk-fixture :as sk]
            [briko.ssh-keygen-fixture :as kf]
            [briko.sshsig :as sshsig]
            [briko.sshsig-test :as vectors]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private v vectors/vectors)

(def ^:private msg (.getBytes ^String (:message v) "UTF-8"))

(defn- pub [k] (str/join " " (take 2 (str/split (get-in v [:keys k :public]) #"\s+"))))

(deftest the-recorded-verdicts-are-ssh-keygen-s
  (when (kf/ssh-keygen? "the-recorded-verdicts-are-ssh-keygen-s")
    (let [dir (fs/create-temp-dir {:prefix "briko-sshsig-"})]
      (try
        (doseq [{:keys [line ssh-keygen]} (:signer-lines v)]
          (is (= ssh-keygen (kf/keygen-verifies? dir line "alice" (:namespace v) msg (get-in v [:signatures :alice])))
              line))
        (testing "and every recorded signature is one ssh-keygen accepts from its key"
          (doseq [k [:alice :mallory :rsa :ecdsa]]
            (is (kf/keygen-verifies? dir (str "alice " (pub k)) "alice" (:namespace v) msg (get-in v [:signatures k]))
                (name k))))
        (finally (fs/delete-tree dir))))))

(deftest it-agrees-with-ssh-keygen-on-fresh-keys
  (when (kf/ssh-keygen? "it-agrees-with-ssh-keygen-on-fresh-keys")
    (let [dir (fs/create-temp-dir {:prefix "briko-sshsig-"})]
      (try
        (doseq [type ["ed25519" "rsa" "ecdsa"]]
          (let [{:keys [key public]} (kf/keygen! dir (str "alice-" type) type)
                line (str "alice namespaces=\"briko-*\" " public)
                armor (:out (process/shell {:in (:message v) :out :string :err :string}
                                           "ssh-keygen" "-Y" "sign" "-f" key "-n" "briko-test"))
                ours (sshsig/verify line msg armor "alice" "briko-test")]
            (is (kf/keygen-verifies? dir line "alice" "briko-test" msg armor) type)
            (if (= "ed25519" type)
              (is (true? ours))
              (is (sshsig/unsupported? ours) type))
            (testing "and both refuse a moved message"
              (let [moved (.getBytes "moved" "UTF-8")]
                (is (not (kf/keygen-verifies? dir line "alice" "briko-test" moved armor)))
                (is (not (true? (sshsig/verify line moved armor "alice" "briko-test"))))))))
        (finally (fs/delete-tree dir))))))

(deftest security-key-signatures-made-in-software-are-ones-ssh-keygen-accepts
  (when (kf/ssh-keygen? "security-key-signatures-made-in-software-are-ones-ssh-keygen-accepts")
    (let [dir (fs/create-temp-dir {:prefix "briko-sshsig-"})]
      (try
        (doseq [s [(sk/signer) (sk/ecdsa-signer)]]
          (let [line (str "alice " (sk/public-line s))]
            (is (kf/keygen-verifies? dir line "alice" "briko-test" msg (sk/sign s "briko-test" msg))
                (:key-type s))
            (testing "ssh-keygen -Y verify does not look at the user-presence flag, briko.sshsig does"
              (let [untouched (sk/sign s "briko-test" msg :flags 0)]
                (is (kf/keygen-verifies? dir line "alice" "briko-test" msg untouched))
                (is (false? (sshsig/verify line msg untouched "alice" "briko-test")))))))
        (testing "a counter changed after signing is refused by both"
          (let [s (sk/signer)
                line (str "alice " (sk/public-line s))
                ^bytes blob (sshsig/unarmor (sk/sign s "briko-test" msg))
                n (alength blob)
                _ (aset-byte blob (dec n) (unchecked-byte (bit-xor 1 (aget blob (dec n)))))
                bent (str "-----BEGIN SSH SIGNATURE-----\n"
                          (.encodeToString (java.util.Base64/getMimeEncoder) blob)
                          "\n-----END SSH SIGNATURE-----\n")]
            (is (not (kf/keygen-verifies? dir line "alice" "briko-test" msg bent)))
            (is (false? (sshsig/verify line msg bent "alice" "briko-test")))))
        (finally (fs/delete-tree dir))))))
