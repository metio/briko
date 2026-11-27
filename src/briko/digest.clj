;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.digest
  "SHA-256 digests of bytes, text, streams and files, and lowercase hex.

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

(defn- sha256-instance
  ^MessageDigest []
  (MessageDigest/getInstance "SHA-256"))

(defn hex
  "The bytes bs as lowercase hex, two digits a byte."
  ^String [bs]
  (when-not (bytes? bs)
    (refuse! ::invalid-input "hex takes a byte array" {}))
  (.formatHex (HexFormat/of) ^bytes bs))

(defn sha256
  "The SHA-256 digest of x, a byte array or a string (as UTF-8), as 32
   bytes."
  ^bytes [x]
  (cond
    (bytes? x) (.digest (sha256-instance) ^bytes x)
    (string? x) (.digest (sha256-instance) (.getBytes ^String x StandardCharsets/UTF_8))
    :else (refuse! ::invalid-input "sha256 takes a byte array or a string; a file goes to sha256-file" {})))

(defn- digest-stream
  ^bytes [^InputStream in]
  (let [md (sha256-instance)
        buffer (byte-array chunk-bytes)]
    (loop []
      (let [n (.read in buffer)]
        (when (pos? n)
          (.update md buffer 0 n))
        (when-not (neg? n)
          (recur))))
    (.digest md)))

(defn sha256-stream
  "The SHA-256 digest of everything left in the InputStream in, as 32
   bytes. Reads in to its end and leaves it open."
  ^bytes [in]
  (when-not (instance? InputStream in)
    (refuse! ::invalid-input "sha256-stream takes an InputStream" {}))
  (try
    (digest-stream in)
    (catch IOException e
      (refuse! ::unreadable (str "the stream cannot be read: " (or (ex-message e) (str e))) {} e))))

(defn sha256-file
  "The SHA-256 digest of the regular file at path, as 32 bytes. Anything
   but a regular file is refused, so a FIFO cannot block the read."
  ^bytes [path]
  (when-not (or (string? path) (instance? java.nio.file.Path path) (instance? java.io.File path))
    (refuse! ::invalid-input "sha256-file takes a string, a Path or a File" {}))
  (let [file (str path)]
    (when-not (fs/regular-file? path)
      (refuse! ::not-a-file (str file " is not a regular file") {:file file}))
    (try
      (with-open [^InputStream in (Files/newInputStream (fs/path path) (make-array OpenOption 0))]
        (digest-stream in))
      (catch IOException e
        (refuse! ::unreadable (str file " cannot be read: " (or (ex-message e) (str e))) {:file file} e)))))

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
