;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.digest-test
  (:require [babashka.fs :as fs]
            [briko.digest :as digest]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.io ByteArrayInputStream IOException InputStream)))

(defn- refusal
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (digest/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- holds?
  [num-tests property]
  (let [result (tc/quick-check num-tests property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :fail :shrunk])))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-digest-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

;; FIPS 180-2 appendix B examples and the NIST CAVS short and long
;; message vectors for SHA-256.
(def ^:private vectors
  [["" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"]
   ["abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"]
   ["abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"]
   [(str "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
         "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
    "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"]])

(def ^:private million-a "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")

(deftest nist-vectors-as-strings-bytes-streams-and-files
  (with-temp-dir [dir]
    (doseq [[message expected] vectors]
      (testing (pr-str message)
        (let [bs (.getBytes ^String message "UTF-8")
              f (str dir "/m")]
          (spit f message)
          (is (= expected (digest/sha256-hex message)))
          (is (= expected (digest/sha256-hex bs)))
          (is (= expected (digest/sha256-hex-stream (ByteArrayInputStream. bs))))
          (is (= expected (digest/sha256-hex-file f)))
          (is (= expected (digest/hex (digest/sha256 message)))))))))

(deftest one-million-a-crosses-many-chunks
  (with-temp-dir [dir]
    (let [bs (byte-array 1000000 (byte 97))
          f (str dir "/a")]
      (fs/write-bytes f bs)
      (is (= million-a (digest/sha256-hex bs)))
      (is (= million-a (digest/sha256-hex-stream (ByteArrayInputStream. bs))))
      (is (= million-a (digest/sha256-hex-file f))))))

(deftest a-near-miss-gives-another-digest
  (is (not= (second (second vectors)) (digest/sha256-hex "abC")))
  (is (not= (second (second vectors)) (digest/sha256-hex "abc\n"))))

(deftest strings-are-digested-as-utf-8
  (is (= (digest/sha256-hex (.getBytes "grüße ✓" "UTF-8")) (digest/sha256-hex "grüße ✓"))))

(deftest hex-is-lowercase-two-digits-a-byte
  (is (= "" (digest/hex (byte-array 0))))
  (is (= "00ff7f80" (digest/hex (byte-array [0 -1 127 -128]))))
  (is (= 32 (count (digest/sha256 "x"))))
  (is (re-matches #"[0-9a-f]{64}" (digest/sha256-hex "x"))))

(defn- tracking-stream
  "An InputStream over bs that records whether it was closed, built with
   proxy over InputStream alone, which babashka supports."
  [^bytes bs closed]
  (let [in (ByteArrayInputStream. bs)]
    (proxy [InputStream] []
      (read
        ([] (.read in))
        ([b] (.read in ^bytes b))
        ([b off len] (.read in ^bytes b (int off) (int len))))
      (close [] (reset! closed true)))))

(deftest a-stream-is-read-to-its-end-and-left-open
  (let [closed (atom false)
        in (tracking-stream (.getBytes "abc" "UTF-8") closed)]
    (is (= (second (second vectors)) (digest/sha256-hex-stream in)))
    (is (= -1 (.read ^InputStream in)))
    (is (false? @closed))))

(deftest refusals
  (with-temp-dir [dir]
    (is (= ::digest/invalid-input (refusal #(digest/sha256 nil))))
    (is (= ::digest/invalid-input (refusal #(digest/sha256 (fs/path dir)))))
    (is (= ::digest/invalid-input (refusal #(digest/hex "ab"))))
    (is (= ::digest/invalid-input (refusal #(digest/sha256-stream "abc"))))
    (is (= ::digest/invalid-input (refusal #(digest/sha256-file nil))))
    (is (= ::digest/not-a-file (refusal #(digest/sha256-file dir))))
    (is (= ::digest/not-a-file (refusal #(digest/sha256-file (str dir "/missing")))))
    (is (= ::digest/unreadable
           (refusal #(digest/sha256-stream (proxy [InputStream] []
                                             (read ([] (throw (IOException. "EIO")))
                                               ([_] (throw (IOException. "EIO")))
                                               ([_ _ _] (throw (IOException. "EIO")))))))))))

(deftest property-streaming-equals-one-shot
  (with-temp-dir [dir]
    (holds? 60
            (prop/for-all [chunks (gen/vector gen/bytes 0 8)
                           big (gen/choose 0 200000)]
              (let [bs (byte-array (concat (apply concat chunks) (repeat big (byte (count chunks)))))
                    f (str dir "/r")
                    one-shot (digest/sha256-hex bs)]
                (fs/write-bytes f bs)
                (= one-shot
                   (digest/sha256-hex-stream (ByteArrayInputStream. bs))
                   (digest/sha256-hex-file f)
                   (digest/hex (.digest (java.security.MessageDigest/getInstance "SHA-256") bs))))))))
