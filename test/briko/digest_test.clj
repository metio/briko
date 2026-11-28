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

;; Known answers. MD5: the RFC 1321 appendix A.5 test suite. SHA-1: FIPS
;; 180-2 appendix A (one- and multi-block messages, one million "a") and
;; the empty message. SHA-512: FIPS 180-2 appendix C and the empty
;; message. Each was also checked against coreutils md5sum, sha1sum and
;; sha512sum.
(def ^:private eighty-digits
  "12345678901234567890123456789012345678901234567890123456789012345678901234567890")

(def ^:private two-block-448 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")

(def ^:private two-block-896
  (str "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
       "ijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))

(def ^:private known-answers
  {:md5 [["" "d41d8cd98f00b204e9800998ecf8427e"]
         ["a" "0cc175b9c0f1b6a831c399e269772661"]
         ["abc" "900150983cd24fb0d6963f7d28e17f72"]
         ["message digest" "f96b697d7cb7938d525a2f31aaf161d0"]
         ["abcdefghijklmnopqrstuvwxyz" "c3fcd3d76192e4007dfb496cca67e13b"]
         ["ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" "d174ab98d277d9f5a5611c2c9f419d9f"]
         [eighty-digits "57edf4a22be3c955ac49da2e2107b67a"]]
   :sha1 [["" "da39a3ee5e6b4b0d3255bfef95601890afd80709"]
          ["abc" "a9993e364706816aba3e25717850c26c9cd0d89d"]
          [two-block-448 "84983e441c3bd26ebaae4aa1f95129e5e54670f1"]]
   :sha256 vectors
   :sha512 [["" (str "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce"
                     "47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")]
            ["abc" (str "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a"
                        "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")]
            [two-block-896 (str "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018"
                                "501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909")]]})

(def ^:private million-a-answers
  {:md5 "7707d6ae4e027c70eea2a935c2296f21"
   :sha1 "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
   :sha256 million-a
   :sha512 (str "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973eb"
                "de0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b")})

(deftest every-checksum-algorithm-has-known-answers
  (is (= #{:md5 :sha1 :sha256 :sha512} digest/checksum-algorithms (set (keys known-answers))))
  (with-temp-dir [dir]
    (doseq [[algorithm answers] known-answers
            [message expected] answers]
      (testing (str algorithm " " (pr-str message))
        (let [bs (.getBytes ^String message "UTF-8")
              f (str dir "/m")]
          (spit f message)
          (is (= expected (digest/checksum-hex algorithm message)))
          (is (= expected (digest/checksum-hex algorithm bs)))
          (is (= expected (digest/checksum-hex-stream algorithm (ByteArrayInputStream. bs))))
          (is (= expected (digest/checksum-hex-file algorithm f))))))))

(deftest sha512-has-known-answers-as-strings-bytes-streams-and-files
  (with-temp-dir [dir]
    (doseq [[message expected] (:sha512 known-answers)]
      (testing (pr-str message)
        (let [bs (.getBytes ^String message "UTF-8")
              f (str dir "/m")]
          (spit f message)
          (is (= expected (digest/sha512-hex message)))
          (is (= expected (digest/sha512-hex bs)))
          (is (= expected (digest/sha512-hex-stream (ByteArrayInputStream. bs))))
          (is (= expected (digest/sha512-hex-file f)))
          (is (= expected (digest/hex (digest/sha512 message))))
          (is (= 64 (count (digest/sha512 bs))))
          (is (= expected (digest/hex (digest/sha512-stream (ByteArrayInputStream. bs)))))
          (is (= expected (digest/hex (digest/sha512-file f)))))))))

(deftest one-million-a-in-every-algorithm-crosses-many-chunks
  (with-temp-dir [dir]
    (let [bs (byte-array 1000000 (byte 97))
          f (str dir "/a")]
      (fs/write-bytes f bs)
      (doseq [[algorithm expected] million-a-answers]
        (testing algorithm
          (is (= expected (digest/checksum-hex algorithm bs)))
          (is (= expected (digest/checksum-hex-stream algorithm (ByteArrayInputStream. bs))))
          (is (= expected (digest/checksum-hex-file algorithm f)))))
      (is (= (:sha512 million-a-answers) (digest/sha512-hex-file f))))))

(deftest checksums-are-lowercase-hex-of-the-algorithms-length
  (doseq [[algorithm digits] {:md5 32 :sha1 40 :sha256 64 :sha512 128}]
    (is (re-matches (re-pattern (str "[0-9a-f]{" digits "}")) (digest/checksum-hex algorithm "x")) (str algorithm))))

(deftest a-near-miss-gives-another-checksum
  (doseq [algorithm digest/checksum-algorithms]
    (is (not= (digest/checksum-hex algorithm "abc") (digest/checksum-hex algorithm "abC")) (str algorithm))))

(deftest a-checksum-stream-is-read-to-its-end-and-left-open
  (let [closed (atom false)
        in (tracking-stream (.getBytes "abc" "UTF-8") closed)]
    (is (= "900150983cd24fb0d6963f7d28e17f72" (digest/checksum-hex-stream :md5 in)))
    (is (= -1 (.read ^InputStream in)))
    (is (false? @closed))))

(deftest checksum-and-sha512-refusals
  (with-temp-dir [dir]
    (let [f (str dir "/m")
          consumed (atom false)
          in (proxy [InputStream] []
               (read ([] (reset! consumed true) -1)
                 ([_] (reset! consumed true) -1)
                 ([_ _ _] (reset! consumed true) -1)))]
      (spit f "abc")
      (doseq [algorithm [nil :sha3 :SHA-256 "md5" "SHA-1" :sha384]]
        (testing (pr-str algorithm)
          (is (= ::digest/invalid-input (refusal #(digest/checksum-hex algorithm "abc"))))
          (is (= ::digest/invalid-input (refusal #(digest/checksum-hex-stream algorithm in))))
          (is (= ::digest/invalid-input (refusal #(digest/checksum-hex-file algorithm f))))))
      (is (false? @consumed) "an unknown algorithm is refused before the stream is read")
      (is (= ::digest/invalid-input (refusal #(digest/checksum-hex :md5 nil))))
      (is (= ::digest/invalid-input (refusal #(digest/checksum-hex-stream :sha1 "abc"))))
      (is (= ::digest/invalid-input (refusal #(digest/checksum-hex-file :sha1 nil))))
      (is (= ::digest/not-a-file (refusal #(digest/checksum-hex-file :md5 dir))))
      (is (= ::digest/not-a-file (refusal #(digest/checksum-hex-file :md5 (str dir "/missing")))))
      (is (= ::digest/invalid-input (refusal #(digest/sha512 nil))))
      (is (= ::digest/invalid-input (refusal #(digest/sha512-stream "abc"))))
      (is (= ::digest/not-a-file (refusal #(digest/sha512-file dir))))
      (is (= ::digest/unreadable
             (refusal #(digest/checksum-hex-stream :md5 (proxy [InputStream] []
                                                          (read ([] (throw (IOException. "EIO")))
                                                            ([_] (throw (IOException. "EIO")))
                                                            ([_ _ _] (throw (IOException. "EIO"))))))))))))

(deftest property-checksum-streaming-equals-one-shot
  (with-temp-dir [dir]
    (holds? 30
            (prop/for-all [algorithm (gen/elements (vec digest/checksum-algorithms))
                           bs gen/bytes
                           big (gen/choose 0 150000)]
              (let [bs (byte-array (concat bs (repeat big (byte 7))))
                    f (str dir "/r")
                    one-shot (digest/checksum-hex algorithm bs)]
                (fs/write-bytes f bs)
                (= one-shot
                   (digest/checksum-hex-stream algorithm (ByteArrayInputStream. bs))
                   (digest/checksum-hex-file algorithm f)))))))
