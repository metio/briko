;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.sshsig-test
  "SSHSIG read in process, against signatures ssh-keygen made (recorded in
   sshsig_vectors.edn) and security-key signatures made in software. These
   tests run no subprocess, so they hold on every runtime and host.

   A verifier's tests are worth more in the negative than the positive: a
   reader that says yes to everything passes every happy path."
  (:require [briko.edn :as edn]
            [briko.sk-fixture :as sk]
            [briko.sshsig :as sshsig]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def vectors (edn/read-file "test/briko/sshsig_vectors.edn"))

(def ^:private ns* (:namespace vectors))

(def ^:private msg (.getBytes ^String (:message vectors) "UTF-8"))

(defn- pub
  "The `type base64` part of a recorded public key line."
  [k]
  (str/join " " (take 2 (str/split (get-in vectors [:keys k :public]) #"\s+"))))

(defn- sig [k] (get-in vectors [:signatures k]))

(def ^:private alice-line (str "alice " (pub :alice)))

(deftest an-honest-signature-verifies
  (is (true? (sshsig/verify alice-line msg (sig :alice) "alice" ns*)))
  (testing "as bytes as well as text"
    (is (true? (sshsig/verify alice-line msg (.getBytes ^String (sig :alice) "UTF-8") "alice" ns*))))
  (testing "and reports who could have produced it"
    (is (= ["alice"] (sshsig/find-principals alice-line msg (sig :alice) ns*)))))

(deftest every-way-it-must-say-no
  (let [mallory-line (str "mallory " (pub :mallory))
        v (fn [text message signature principal namespace]
            (sshsig/verify text message signature principal namespace))]
    (testing "a message that moved"
      (is (false? (v alice-line (.getBytes "the bytes that were endorsed!" "UTF-8") (sig :alice) "alice" ns*))))
    (testing "a principal the key is not registered to"
      (is (false? (v alice-line msg (sig :alice) "mallory" ns*))))
    (testing "a namespace the signature was not made for"
      (is (false? (v alice-line msg (sig :alice) "alice" "briko-other")))
      (is (false? (v alice-line msg (sig :alice-other-namespace) "alice" ns*))))
    (testing "a registry that does not name the key"
      (is (false? (v mallory-line msg (sig :alice) "alice" ns*)))
      (is (false? (v "" msg (sig :alice) "alice" ns*)))
      (is (false? (v nil msg (sig :alice) "alice" ns*))))
    (testing "a namespaces= option that excludes it"
      (is (false? (v (str "alice namespaces=\"other-*\" " (pub :alice)) msg (sig :alice) "alice" ns*)))
      (is (true? (v (str "alice namespaces=\"briko-te*\" " (pub :alice)) msg (sig :alice) "alice" ns*))))
    (testing "armor that is not a signature at all"
      (doseq [junk [nil "" "not a signature" "-----BEGIN SSH SIGNATURE-----\n@@\n"
                    (str/replace (sig :alice) "-----BEGIN" "-----NOPE")]]
        (is (false? (v alice-line msg junk "alice" ns*)) (pr-str junk))))
    (testing "a truncated blob is refused rather than read short"
      (let [body (str/split-lines (sig :alice))
            cut (str/join "\n" (concat (take 3 body) ["-----END SSH SIGNATURE-----"]))]
        (is (false? (v alice-line msg cut "alice" ns*)))))
    (testing "a signature that is valid for someone else"
      (is (false? (v (str alice-line "\n" mallory-line) msg (sig :mallory) "alice" ns*)))
      (is (true? (v (str alice-line "\n" mallory-line) msg (sig :mallory) "mallory" ns*))))
    (testing "a principal that is missing or blank never stands for any principal"
      (doseq [principal [nil "" "  " :alice]]
        (is (false? (v alice-line msg (sig :alice) principal ns*)) (pr-str principal))))))

(deftest it-never-grants-what-ssh-keygen-refuses
  ;; :ssh-keygen is ssh-keygen's recorded verdict for the same line; the
  ;; oracle test re-checks each against the ssh-keygen on the host
  (doseq [{:keys [line ssh-keygen]} (:signer-lines vectors)]
    (let [ours (sshsig/verify line msg (sig :alice) "alice" ns*)]
      (is (or (sshsig/unsupported? ours) (= ssh-keygen ours))
          (str line " - ssh-keygen " ssh-keygen ", in process " ours))))
  (testing "the options that need a clock defer to ssh-keygen"
    (doseq [opt ["valid-before=\"20200101\"" "valid-after=\"20200101\""]]
      (is (sshsig/unsupported? (sshsig/verify (str "alice " opt " " (pub :alice)) msg (sig :alice) "alice" ns*)))))
  (testing "find-principals agrees"
    (is (= [] (sshsig/find-principals (str "alice cert-authority " (pub :alice)) msg (sig :alice) ns*)))
    (is (= [] (sshsig/find-principals (str "alice namespaces=\"!briko-test,briko-*\" " (pub :alice)) msg (sig :alice) ns*)))
    (is (= ["bob" "alice"] (sshsig/find-principals (str "bob,alice " (pub :alice)) msg (sig :alice) ns*)))))

(deftest key-types-it-does-not-implement-are-unsupported-not-forged
  (doseq [k [:rsa :ecdsa]]
    (let [line (str "alice " (pub k))]
      (is (sshsig/unsupported? (sshsig/verify line msg (sig k) "alice" ns*)) (name k))
      (is (sshsig/unsupported? (sshsig/find-principals line msg (sig k) ns*)) (name k))
      (testing "but still false when the namespace or the principal is wrong"
        (is (false? (sshsig/verify line msg (sig k) "alice" "briko-other")))
        (is (false? (sshsig/verify line msg (sig k) "mallory" ns*)))))))

(deftest signer-lines-are-read-as-openssh-reads-them
  (testing "an option without a name is kept as an unknown option, which refuses the line"
    (is (= [["=" nil]] (:options (sshsig/parse-signer-line "alice = ssh-ed25519 AAAAC3x")))))
  (is (= ["alice"] (sshsig/principals (str "alice = " (pub :alice)))))
  (is (= ["bob" "alice"] (sshsig/principals (str "bob,alice " (pub :alice)))))
  (is (= [["namespaces" "briko-*"]] (:options (sshsig/parse-signer-line (str "alice namespaces=\"briko-*\" " (pub :alice))))))
  (testing "text in the comment never counts as an option or a key"
    (is (= [] (:options (sshsig/parse-signer-line (str alice-line " cert-authority"))))))
  (doseq [l [nil "" "   " "# alice ssh-ed25519 AAAAC3x" "alice" "alice ssh-ed25519" "alice ssh-ed25519 notbase64"]]
    (is (nil? (sshsig/parse-signer-line l)) (pr-str l))))

(deftest it-reads-the-pieces-of-a-signature
  (is (= "ssh-ed25519" (sshsig/signer-key-type (sig :alice))))
  (is (= "ssh-rsa" (sshsig/signer-key-type (sig :rsa))))
  (is (nil? (sshsig/signer-key-type "junk")))
  (let [parsed (sshsig/parse (sshsig/unarmor (sig :alice)))]
    (is (= ns* (:namespace parsed)))
    (is (= "sha512" (:hash-algorithm parsed)))
    (is (= (seq (sshsig/key-blob (pub :alice))) (seq (:public-key parsed)))))
  (is (nil? (sshsig/parse (byte-array 3))))
  (is (nil? (sshsig/parse nil)))
  (is (nil? (sshsig/unarmor "no armor here")))
  (is (nil? (sshsig/key-blob "ssh-ed25519")))
  (is (nil? (sshsig/key-blob nil))))

(deftest invalid-arguments-are-refused
  (doseq [[message namespace] [[nil ns*] ["text" ns*] [msg nil] [msg ""] [msg "  "] [msg :briko-test]]]
    (let [e (try (sshsig/verify alice-line message (sig :alice) "alice" namespace) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (sshsig/refusal? e) (pr-str [message namespace]))
      (is (= ::sshsig/invalid-arguments (:type (ex-data e)))))
    (is (thrown? clojure.lang.ExceptionInfo (sshsig/find-principals alice-line message (sig :alice) namespace)))))

;; Security keys, signed in software by briko.sk-fixture

(def ^:private sk-ns "briko-test")

(defn- sk-line [signer] (str "alice " (sk/public-line signer)))

(defn- flip-last-byte
  "The armored signature with the final byte of its blob changed: the low
   byte of the counter, which the signature covers."
  [armor]
  (let [^bytes blob (sshsig/unarmor armor)
        n (alength blob)]
    (aset-byte blob (dec n) (unchecked-byte (bit-xor 1 (aget blob (dec n)))))
    (str "-----BEGIN SSH SIGNATURE-----\n"
         (.encodeToString (java.util.Base64/getMimeEncoder) blob)
         "\n-----END SSH SIGNATURE-----\n")))

(deftest a-touched-security-key-signature-verifies-in-process
  (let [s (sk/signer)
        armor (sk/sign s sk-ns msg)]
    (is (true? (sshsig/verify (sk-line s) msg armor "alice" sk-ns)))
    (is (= ["alice"] (sshsig/find-principals (sk-line s) msg armor sk-ns)))
    (is (= sk/key-type (sshsig/signer-key-type armor)))))

(deftest a-security-key-signature-without-a-touch-counts-for-nothing
  (let [s (sk/signer)
        armor (sk/sign s sk-ns msg :flags 0)]
    (is (false? (sshsig/verify (sk-line s) msg armor "alice" sk-ns)))
    (is (= [] (sshsig/find-principals (sk-line s) msg armor sk-ns)))
    (testing "also when only the user-verified flag is set"
      (is (false? (sshsig/verify (sk-line s) msg (sk/sign s sk-ns msg :flags sk/flag-user-verified) "alice" sk-ns))))))

(deftest a-pin-verified-signature-is-also-a-touched-one
  (let [s (sk/signer)
        armor (sk/sign s sk-ns msg :flags (bit-or sk/flag-user-present sk/flag-user-verified))]
    (is (true? (sshsig/verify (sk-line s) msg armor "alice" sk-ns)))))

(deftest every-way-a-security-key-signature-must-say-no
  (let [s (sk/signer)
        other (sk/signer)
        armor (sk/sign s sk-ns msg)]
    (testing "a message that moved"
      (is (false? (sshsig/verify (sk-line s) (.getBytes "other" "UTF-8") armor "alice" sk-ns))))
    (testing "a counter changed after signing"
      (is (false? (sshsig/verify (sk-line s) msg (flip-last-byte armor) "alice" sk-ns))))
    (testing "another key's line"
      (is (false? (sshsig/verify (sk-line other) msg armor "alice" sk-ns))))
    (testing "another namespace"
      (is (false? (sshsig/verify (sk-line s) msg armor "alice" "briko-other"))))
    (testing "the application is part of what was signed"
      (let [moved (assoc s :application "ssh:other" :blob (:blob (sk/signer "ssh:other")))]
        (is (false? (sshsig/verify (sk-line moved) msg armor "alice" sk-ns)))))))

(deftest an-ecdsa-security-key-is-judged-on-its-touch-before-it-is-unsupported
  (let [s (sk/ecdsa-signer)]
    (testing "untouched: refused here, so a fallback to ssh-keygen never sees it"
      (is (false? (sshsig/verify (sk-line s) msg (sk/sign s sk-ns msg :flags 0) "alice" sk-ns)))
      (is (= [] (sshsig/find-principals (sk-line s) msg (sk/sign s sk-ns msg :flags 0) sk-ns))))
    (testing "touched: left to ssh-keygen"
      (is (sshsig/unsupported? (sshsig/verify (sk-line s) msg (sk/sign s sk-ns msg) "alice" sk-ns)))
      (is (sshsig/unsupported? (sshsig/find-principals (sk-line s) msg (sk/sign s sk-ns msg) sk-ns))))))

(deftest a-signer-line-with-a-clock-option-does-not-reopen-the-touch
  (let [s (sk/signer)
        dated (str "alice valid-after=\"20200101\" " (sk/public-line s))]
    (is (false? (sshsig/verify dated msg (sk/sign s sk-ns msg :flags 0) "alice" sk-ns)))
    (is (sshsig/unsupported? (sshsig/verify dated msg (sk/sign s sk-ns msg) "alice" sk-ns)))))
