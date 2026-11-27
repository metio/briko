;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.ssh-sign-test
  "Signing through ssh-keygen and verifying in process with the
   ssh-keygen fallback. The tests that sign or fall back need ssh-keygen
   (briko.ssh-keygen-fixture reports a missing one); the rest point
   :ssh-keygen at a program that does not exist, which proves they never
   fork."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [briko.sk-fixture :as sk]
            [briko.ssh-keygen-fixture :as kf]
            [briko.ssh-sign :as ssh-sign]
            [briko.sshsig-test :as vectors]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private v vectors/vectors)

(def ^:private msg (.getBytes ^String (:message v) "UTF-8"))

(def ^:private ns* (:namespace v))

(def ^:private no-ssh-keygen
  "Options naming a program that cannot be started."
  {:ssh-keygen "/nonexistent/briko/ssh-keygen"})

(defn- pub [k] (str/join " " (take 2 (str/split (get-in v [:keys k :public]) #"\s+"))))

(defn- sig [k] (get-in v [:signatures k]))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (fs/create-temp-dir {:prefix "briko-sign-test-"})]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- refusal-type
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (when (ssh-sign/refusal? e) (:type (ex-data e))))))

(defn- leftover-directories
  "The temporary directories ssh-sign leaves behind, which must be none."
  []
  (set (map str (fs/glob (fs/temp-dir) "briko-ssh-sign*" {:max-depth 1}))))

(defn- fake-ssh-keygen!
  "An executable standing in for ssh-keygen that runs the shell script body."
  [dir body]
  (let [p (fs/path dir "fake-ssh-keygen")]
    (spit (str p) (str "#!/bin/sh\n" body "\n"))
    (fs/set-posix-file-permissions p "rwx------")
    (str p)))

;; In process, no ssh-keygen

(deftest fingerprints-are-the-ones-ssh-keygen-prints
  (is (= (get-in v [:keys :alice :fingerprint]) (ssh-sign/fingerprint (get-in v [:keys :alice :public]))))
  (is (= (get-in v [:keys :alice :fingerprint]) (ssh-sign/fingerprint (pub :alice))))
  (doseq [bad [nil "" "ssh-ed25519" "not a key" :key]]
    (is (= ::ssh-sign/invalid-arguments (refusal-type #(ssh-sign/fingerprint bad))) (pr-str bad))))

(deftest supported-signatures-are-judged-without-forking
  (with-temp-dir [dir]
    (let [signers (str (fs/path dir "allowed"))]
      (spit signers (str "alice " (pub :alice) "\n"))
      (is (true? (ssh-sign/verify-bytes signers msg (sig :alice) "alice" ns* no-ssh-keygen)))
      (is (true? (ssh-sign/verify-bytes (fs/path signers) msg (.getBytes ^String (sig :alice) "UTF-8") "alice" ns* no-ssh-keygen)))
      (is (true? (ssh-sign/verify-text (str "alice " (pub :alice)) msg (sig :alice) "alice" ns* no-ssh-keygen)))
      (is (= ["alice"] (ssh-sign/find-principals signers msg (sig :alice) ns* no-ssh-keygen)))
      (testing "and refused without forking"
        (is (false? (ssh-sign/verify-bytes signers (.getBytes "moved" "UTF-8") (sig :alice) "alice" ns* no-ssh-keygen)))
        (is (false? (ssh-sign/verify-bytes signers msg (sig :alice) "mallory" ns* no-ssh-keygen)))
        (is (false? (ssh-sign/verify-bytes signers msg (sig :alice) nil ns* no-ssh-keygen)))
        (is (false? (ssh-sign/verify-text (str "alice " (pub :alice)) msg (sig :alice) "alice" "briko-other" no-ssh-keygen)))
        (is (= [] (ssh-sign/find-principals signers msg (sig :mallory) ns* no-ssh-keygen))))
      (testing "a touched security key"
        (let [s (sk/signer)
              line (str "alice " (sk/public-line s))]
          (is (true? (ssh-sign/verify-text line msg (sk/sign s ns* msg) "alice" ns* no-ssh-keygen)))
          (is (false? (ssh-sign/verify-text line msg (sk/sign s ns* msg :flags 0) "alice" ns* no-ssh-keygen))))))))

(deftest an-unsupported-signature-without-ssh-keygen-is-unchecked-not-forged
  (with-temp-dir [dir]
    (let [signers (str (fs/path dir "allowed"))]
      (spit signers (str "alice " (pub :rsa) "\n"))
      (is (nil? (ssh-sign/verify-bytes signers msg (sig :rsa) "alice" ns* no-ssh-keygen)))
      (is (nil? (ssh-sign/verify-text (str "alice " (pub :rsa)) msg (sig :rsa) "alice" ns* no-ssh-keygen)))
      (is (nil? (ssh-sign/find-principals signers msg (sig :rsa) ns* no-ssh-keygen)))
      (is (false? (ssh-sign/ssh-keygen-available? no-ssh-keygen)))
      (testing "an untouched ECDSA security key is refused before any fallback"
        (let [s (sk/ecdsa-signer)
              line (str "alice " (sk/public-line s))]
          (is (false? (ssh-sign/verify-text line msg (sk/sign s ns* msg :flags 0) "alice" ns* no-ssh-keygen)))
          (is (nil? (ssh-sign/verify-text line msg (sk/sign s ns* msg) "alice" ns* no-ssh-keygen))))))))

(deftest invalid-arguments-and-options-are-refused
  (let [line (str "alice " (pub :alice))]
    (doseq [[label f] [["nil message" #(ssh-sign/verify-text line nil (sig :alice) "alice" ns*)]
                       ["string message" #(ssh-sign/verify-bytes "/x" "text" (sig :alice) "alice" ns*)]
                       ["blank namespace" #(ssh-sign/verify-text line msg (sig :alice) "alice" " ")]
                       ["keyword signature" #(ssh-sign/verify-text line msg :sig "alice" ns*)]
                       ["nil registry text" #(ssh-sign/verify-text nil msg (sig :alice) "alice" ns*)]
                       ["blank registry path" #(ssh-sign/verify-bytes "" msg (sig :alice) "alice" ns*)]
                       ["nil key file" #(ssh-sign/sign-bytes nil msg ns*)]
                       ["nil sign namespace" #(ssh-sign/sign-bytes "/k" msg nil)]
                       ["string sign message" #(ssh-sign/sign-bytes "/k" "text" ns*)]
                       ["find-principals namespace" #(ssh-sign/find-principals "/x" msg (sig :alice) nil)]]]
      (is (= ::ssh-sign/invalid-arguments (refusal-type f)) label))
    (doseq [opts [{:timeout-ms 0} {:timeout-ms "5"} {:ssh-keygen ""} {:deadline-ms 5} [:timeout-ms 5]]]
      (is (= ::ssh-sign/invalid-options
             (refusal-type #(ssh-sign/verify-text line msg (sig :alice) "alice" ns* opts)))
          (pr-str opts)))))

(deftest failures-to-sign-are-refusals
  (with-temp-dir [dir]
    (testing "a program that cannot be started"
      (is (= ::ssh-sign/command-not-run (refusal-type #(ssh-sign/sign-bytes "/k" msg ns* no-ssh-keygen))))
      (is (= ::ssh-sign/command-not-run (refusal-type #(ssh-sign/pubkey "/k" no-ssh-keygen)))))
    (testing "a program that fails"
      (let [fake (fake-ssh-keygen! dir "echo 'no such key' >&2; exit 255")
            e (try (ssh-sign/sign-bytes "/k" msg ns* {:ssh-keygen fake}) nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= ::ssh-sign/command-failed (:type (ex-data e))))
        (is (= 255 (:exit (ex-data e))))
        (is (= "no such key" (:err (ex-data e))))))
    (testing "a program that overruns its deadline"
      (let [fake (fake-ssh-keygen! dir "exec sleep 30")]
        (is (= ::ssh-sign/command-timed-out
               (refusal-type #(ssh-sign/sign-bytes "/k" msg ns* {:ssh-keygen fake :timeout-ms 300}))))))
    (testing "a program that prints something other than a signature or a key"
      (let [fake (fake-ssh-keygen! dir "cat >/dev/null; echo hello")]
        (is (= ::ssh-sign/invalid-output (refusal-type #(ssh-sign/sign-bytes "/k" msg ns* {:ssh-keygen fake}))))
        (is (= ::ssh-sign/invalid-output (refusal-type #(ssh-sign/pubkey "/k" {:ssh-keygen fake}))))))
    (testing "an ssh-keygen that hangs while verifying leaves the verdict unchecked"
      (let [fake (fake-ssh-keygen! dir "case \"$2\" in check-novalidate) exit 255;; esac; exec sleep 30")
            signers (str (fs/path dir "allowed"))]
        (spit signers (str "alice " (pub :rsa) "\n"))
        (is (nil? (ssh-sign/verify-bytes signers msg (sig :rsa) "alice" ns* {:ssh-keygen fake :timeout-ms 300})))))))

;; With ssh-keygen

(deftest it-signs-and-verifies-through-ssh-keygen
  (when (kf/ssh-keygen? "it-signs-and-verifies-through-ssh-keygen")
    (with-temp-dir [dir]
      (let [before (leftover-directories)
            {:keys [key public]} (kf/keygen! dir "alice" "ed25519")
            line (str "alice namespaces=\"briko-*\" " (ssh-sign/pubkey key))
            signers (str (fs/path dir "allowed"))
            armor (ssh-sign/sign-bytes key msg "briko-test")]
        (spit signers (str line "\n"))
        (is (= public (ssh-sign/pubkey key)))
        (is (str/starts-with? (String. ^bytes armor "UTF-8") "-----BEGIN SSH SIGNATURE-----"))
        (is (kf/keygen-verifies? dir line "alice" "briko-test" msg (String. ^bytes armor "UTF-8")))
        (is (true? (ssh-sign/verify-bytes signers msg armor "alice" "briko-test")))
        (is (true? (ssh-sign/verify-text line msg armor "alice" "briko-test")))
        (is (false? (ssh-sign/verify-bytes signers msg armor "alice" "briko-other")))
        (testing "the namespace is part of what was signed"
          (is (false? (ssh-sign/verify-bytes signers msg (ssh-sign/sign-bytes key msg "briko-other") "alice" "briko-test"))))
        (testing "the fingerprint is the one ssh-keygen -l prints"
          (let [printed (second (str/split (:out (process/shell {:out :string} "ssh-keygen" "-l" "-E" "sha256" "-f" (str key ".pub"))) #"\s+"))]
            (is (= printed (ssh-sign/fingerprint public)))))
        (is (= before (leftover-directories)) "no temporary directory is left behind")))))

(deftest unsupported-key-types-fall-back-to-ssh-keygen
  (when (kf/ssh-keygen? "unsupported-key-types-fall-back-to-ssh-keygen")
    (with-temp-dir [dir]
      (let [before (leftover-directories)]
        (doseq [type ["rsa" "ecdsa"]]
          (let [{:keys [key public]} (kf/keygen! dir (str "alice-" type) type)
                line (str "alice " public)
                signers (str (fs/path dir (str "allowed-" type)))
                armor (ssh-sign/sign-bytes key msg ns*)]
            (spit signers (str line "\n"))
            (is (true? (ssh-sign/verify-bytes signers msg armor "alice" ns*)) type)
            (is (true? (ssh-sign/verify-text line msg armor "alice" ns*)) type)
            (is (= ["alice"] (ssh-sign/find-principals signers msg armor ns*)) type)
            (is (false? (ssh-sign/verify-bytes signers (.getBytes "moved" "UTF-8") armor "alice" ns*)) type)
            (is (false? (ssh-sign/verify-text line msg armor "mallory" ns*)) type)))
        (testing "a registry path that is not a file is judged by ssh-keygen, which refuses"
          (is (false? (ssh-sign/verify-bytes (str (fs/path dir "missing")) msg (sig :alice) "alice" ns*))))
        (testing "the recorded signatures"
          (is (true? (ssh-sign/verify-text (str "alice " (pub :rsa)) msg (sig :rsa) "alice" ns*)))
          (is (true? (ssh-sign/verify-text (str "alice " (pub :ecdsa)) msg (sig :ecdsa) "alice" ns*))))
        (is (= before (leftover-directories)) "no temporary directory is left behind")))))

(defn- every-reader
  "What each verifying function says about armor under the allowed
   signers text."
  [dir allowed armor]
  (let [f (str (fs/path dir (str "signers-" (System/nanoTime))))]
    (spit f (str allowed "\n"))
    {:verify-text (ssh-sign/verify-text allowed msg armor "alice" ns*)
     :verify-bytes (ssh-sign/verify-bytes f msg armor "alice" ns*)
     :find-principals (ssh-sign/find-principals f msg armor ns*)}))

(deftest an-ecdsa-security-key-signature-without-a-touch-counts-for-nothing
  (when (kf/ssh-keygen? "an-ecdsa-security-key-signature-without-a-touch-counts-for-nothing")
    (with-temp-dir [dir]
      (let [s (sk/ecdsa-signer)
            line (str "alice " (sk/public-line s))]
        (is (= {:verify-text false :verify-bytes false :find-principals []}
               (every-reader dir line (sk/sign s ns* msg :flags 0))))
        (testing "while a touched one still verifies through ssh-keygen"
          (is (= {:verify-text true :verify-bytes true :find-principals ["alice"]}
                 (every-reader dir line (sk/sign s ns* msg)))))))))

(deftest a-signer-line-with-a-clock-option-does-not-reopen-the-touch
  (when (kf/ssh-keygen? "a-signer-line-with-a-clock-option-does-not-reopen-the-touch")
    (with-temp-dir [dir]
      (let [s (sk/signer)
            dated (str "alice valid-after=\"20200101\" " (sk/public-line s))]
        (is (= {:verify-text false :verify-bytes false :find-principals []}
               (every-reader dir dated (sk/sign s ns* msg :flags 0))))
        (is (= {:verify-text true :verify-bytes true :find-principals ["alice"]}
               (every-reader dir dated (sk/sign s ns* msg))))
        (testing "and an expired line is refused by ssh-keygen"
          (is (false? (:verify-text (every-reader dir (str "alice valid-before=\"20200101\" " (sk/public-line s))
                                                  (sk/sign s ns* msg))))))))))
