;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.digest
  "SHA-256 and SHA-512 digests of bytes, text, streams and files,
   checksums for verifying published MD5 and SHA-1 sums, and lowercase
   hex.

   MD5 and SHA-1 are broken for collisions, so they are offered only
   through `checksum-hex` and its stream and file forms, named for the
   one use left to them: comparing a download with the checksum its
   publisher printed in that algorithm. Anything briko or its callers
   hash for their own records uses SHA-256 or SHA-512.

   Streams and files are digested in chunks of 64 KiB, so a digest takes
   the same memory whatever the size of its input. Text is digested as
   its UTF-8 bytes.

   The hex form carries no prefix. The formats that print one disagree
   (`sha256-<hex>` names tik events and kajero records, `sha256:<hex>`
   names files in a skill manifest), and each is frozen by the hashes and
   signatures already made in it, so the caller that owns a format adds
   its own prefix.

   Every refusal is an ex-info whose data carries a stable :type, one of
   `refusal-types`."
  (:require [babashka.fs :as fs])
  (:import (java.io IOException InputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files OpenOption)
           (java.security MessageDigest)
           (java.util HexFormat)))

(set! *warn-on-reflection* true)

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-input ::not-a-file ::unreadable})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

(def ^:private chunk-bytes 65536)

(def ^:private algorithm-names
  {:md5 "MD5" :sha1 "SHA-1" :sha256 "SHA-256" :sha512 "SHA-512"})

(def checksum-algorithms
  "The algorithms `checksum-hex` and its stream and file forms take."
  (set (keys algorithm-names)))

(defn- instance
  ^MessageDigest [algorithm]
  (if-let [n (get algorithm-names algorithm)]
    (MessageDigest/getInstance ^String n)
    (refuse! ::invalid-input (str "unknown checksum algorithm " (pr-str algorithm)
                                  "; one of " (pr-str (sort checksum-algorithms)))
             {:algorithm algorithm})))

(defn hex
  "The bytes bs as lowercase hex, two digits a byte."
  ^String [bs]
  (when-not (bytes? bs)
    (refuse! ::invalid-input "hex takes a byte array" {}))
  (.formatHex (HexFormat/of) ^bytes bs))

(defn- digest-bytes
  ^bytes [algorithm fname x]
  (let [md (instance algorithm)]
    (cond
      (bytes? x) (.digest md ^bytes x)
      (string? x) (.digest md (.getBytes ^String x StandardCharsets/UTF_8))
      :else (refuse! ::invalid-input (str fname " takes a byte array or a string; a file goes to " fname "-file") {}))))

(defn- digest-stream
  ^bytes [algorithm ^InputStream in]
  (let [md (instance algorithm)
        buffer (byte-array chunk-bytes)]
    (loop []
      (let [n (.read in buffer)]
        (when (pos? n)
          (.update md buffer 0 n))
        (when-not (neg? n)
          (recur))))
    (.digest md)))

(defn- digest-input-stream
  ^bytes [algorithm fname in]
  (when-not (instance? InputStream in)
    (refuse! ::invalid-input (str fname "-stream takes an InputStream") {}))
  (instance algorithm)
  (try
    (digest-stream algorithm in)
    (catch IOException e
      (refuse! ::unreadable (str "the stream cannot be read: " (or (ex-message e) (str e))) {} e))))

(defn- digest-file
  ^bytes [algorithm fname path]
  (when-not (or (string? path) (instance? java.nio.file.Path path) (instance? java.io.File path))
    (refuse! ::invalid-input (str fname "-file takes a string, a Path or a File") {}))
  (instance algorithm)
  (let [file (str path)]
    (when-not (fs/regular-file? path)
      (refuse! ::not-a-file (str file " is not a regular file") {:file file}))
    (try
      (with-open [^InputStream in (Files/newInputStream (fs/path path) (make-array OpenOption 0))]
        (digest-stream algorithm in))
      (catch IOException e
        (refuse! ::unreadable (str file " cannot be read: " (or (ex-message e) (str e))) {:file file} e)))))

(defn sha256
  "The SHA-256 digest of x, a byte array or a string (as UTF-8), as 32
   bytes."
  ^bytes [x]
  (digest-bytes :sha256 "sha256" x))

(defn sha256-stream
  "The SHA-256 digest of everything left in the InputStream in, as 32
   bytes. Reads in to its end and leaves it open."
  ^bytes [in]
  (digest-input-stream :sha256 "sha256" in))

(defn sha256-file
  "The SHA-256 digest of the regular file at path, as 32 bytes. Anything
   but a regular file is refused, so a FIFO cannot block the read."
  ^bytes [path]
  (digest-file :sha256 "sha256" path))

(defn sha256-hex
  "The SHA-256 digest of x, a byte array or a string (as UTF-8), as 64
   lowercase hex digits."
  ^String [x]
  (hex (sha256 x)))

(defn sha256-hex-stream
  "The SHA-256 digest of the rest of the InputStream in as 64 lowercase
   hex digits; see `sha256-stream`."
  ^String [in]
  (hex (sha256-stream in)))

(defn sha256-hex-file
  "The SHA-256 digest of the regular file at path as 64 lowercase hex
   digits; see `sha256-file`."
  ^String [path]
  (hex (sha256-file path)))

(defn sha512
  "The SHA-512 digest of x, a byte array or a string (as UTF-8), as 64
   bytes."
  ^bytes [x]
  (digest-bytes :sha512 "sha512" x))

(defn sha512-stream
  "The SHA-512 digest of everything left in the InputStream in, as 64
   bytes. Reads in to its end and leaves it open."
  ^bytes [in]
  (digest-input-stream :sha512 "sha512" in))

(defn sha512-file
  "The SHA-512 digest of the regular file at path, as 64 bytes. Anything
   but a regular file is refused, so a FIFO cannot block the read."
  ^bytes [path]
  (digest-file :sha512 "sha512" path))

(defn sha512-hex
  "The SHA-512 digest of x, a byte array or a string (as UTF-8), as 128
   lowercase hex digits."
  ^String [x]
  (hex (sha512 x)))

(defn sha512-hex-stream
  "The SHA-512 digest of the rest of the InputStream in as 128 lowercase
   hex digits; see `sha512-stream`."
  ^String [in]
  (hex (sha512-stream in)))

(defn sha512-hex-file
  "The SHA-512 digest of the regular file at path as 128 lowercase hex
   digits; see `sha512-file`."
  ^String [path]
  (hex (sha512-file path)))

;; Published checksums

(defn checksum-hex
  "The checksum of x, a byte array or a string (as UTF-8), in algorithm,
   one of `checksum-algorithms` (:md5, :sha1, :sha256, :sha512), as
   lowercase hex. For comparing with a checksum a publisher printed; MD5
   and SHA-1 prove nothing against a party who chose the content."
  ^String [algorithm x]
  (hex (digest-bytes algorithm "checksum-hex" x)))

(defn checksum-hex-stream
  "The checksum of the rest of the InputStream in, in algorithm, as
   lowercase hex; reads in to its end and leaves it open. See
   `checksum-hex`."
  ^String [algorithm in]
  (hex (digest-input-stream algorithm "checksum-hex" in)))

(defn checksum-hex-file
  "The checksum of the regular file at path, in algorithm, as lowercase
   hex. Anything but a regular file is refused. See `checksum-hex`."
  ^String [algorithm path]
  (hex (digest-file algorithm "checksum-hex" path)))
