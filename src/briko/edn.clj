;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.edn
  "Reads EDN from text that may come from anywhere, as data and nothing
   else, within bounds the caller sets.

   clojure.edn reads the text, never the Clojure reader, so read-eval,
   reader conditionals, syntax quote, regexes and anonymous functions are
   refused as syntax. On top of that:

     - the input is at most :max-bytes long in UTF-8 and is valid UTF-8;
     - a linear scan runs before the reader and refuses text that nests
       deeper than :max-depth, so a deep form never reaches the recursive
       reader, whose StackOverflowError a `catch Exception` would miss.
       Collections count one level each, and so do the prefixes the reader
       also recurses on: tagged literals, metadata and discards. A
       namespaced map counts two, its prefix and its map;
     - a number literal is at most :max-number-chars long, because parsing
       a long integer literal takes time quadratic in its length;
     - a tagged literal is read only when :readers names its tag, #inst
       and #uuid included, because a tag reader is code that runs while
       the text is read;
     - the text holds exactly one form unless :one-form? is false, and
       every form satisfies :expect when it is given.

   Every refusal is an ex-info whose data carries a stable :type, one of
   the keywords in `refusal-types`, so a caller can branch on it without
   parsing the message."
  (:refer-clojure :exclude [read-string])
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io IOException InputStream PushbackReader StringReader)
           (java.nio ByteBuffer CharBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a read uses where the caller gives none.

   - :max-bytes, the longest input in UTF-8 bytes: 1 MiB.
   - :max-depth, the deepest nesting of collections and prefixes: 64.
   - :max-number-chars, the longest number literal: 100 characters, which
     holds every long, double and realistic decimal.
   - :readers, a map from tag symbol to reader function: none, so every
     tagged literal is refused.
   - :one-form?, whether the text holds exactly one form: true.
   - :expect, a predicate every form must satisfy: none."
  {:max-bytes 1048576
   :max-depth 64
   :max-number-chars 100
   :readers {}
   :one-form? true
   :expect nil})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-input ::too-large ::invalid-utf8 ::too-deep
    ::number-too-long ::tag-refused ::tag-failed ::malformed ::no-form
    ::trailing-form ::unexpected ::not-a-file ::unreadable})

(defn- refuse!
  ([type message] (refuse! type message {} nil))
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

;; Options

(defn- pos-int-below?
  [limit x]
  (and (int? x) (pos? (long x)) (< (long x) (long limit))))

(def ^:private option-checks
  ;; :max-bytes stays below Integer/MAX_VALUE because the stream read asks
  ;; for one byte more than the limit in an int.
  {:max-bytes [(partial pos-int-below? Integer/MAX_VALUE) "a positive int below Integer/MAX_VALUE"]
   :max-depth [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   :max-number-chars [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   :readers [(fn [m] (and (map? m) (every? (fn [[k v]] (and (symbol? k) (ifn? v))) m)))
             "a map from tag symbol to function"]
   :one-form? [boolean? "a boolean"]
   :expect [(fn [p] (or (nil? p) (ifn? p))) "nil or a predicate"]})

(defn- options
  [opts]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {:options opts}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k :value (get opts k)})))
    opts))

;; Bytes and characters

(defn- too-large!
  [max-bytes]
  (refuse! ::too-large (format "the input is larger than %d bytes" max-bytes) {:limit max-bytes}))

(defn- decode-utf-8
  "The text bytes hold, refusing malformed or unmappable input."
  ^String [^bytes bs]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (try
      (str (.decode decoder (ByteBuffer/wrap bs)))
      (catch CharacterCodingException e
        (refuse! ::invalid-utf8 "the input is not valid UTF-8" {} e)))))

(defn- utf-8-length
  "The length of s in UTF-8, refusing a string holding an unpaired
   surrogate, which has no UTF-8 encoding."
  ^long [^String s]
  (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (try
      (.remaining ^ByteBuffer (.encode encoder (CharBuffer/wrap s)))
      (catch CharacterCodingException e
        (refuse! ::invalid-utf8 "the input holds an unpaired surrogate and is not valid UTF-8" {} e)))))

;; The scan
;;
;; The scan mirrors how clojure.edn's EdnReader recurses, so that the
;; frames it counts bound the reader's call depth. It has to be exact up
;; to the first syntax error only: the reader stops there, so nothing
;; after that point is ever read. It may count more frames than the reader
;; uses, never fewer.
;;
;; A frame is a collection, which ends at its closing delimiter, or a
;; prefix the reader handles by reading further forms inside the same call:
;;
;;   #tag x    one form, and the tagged literal is a form itself
;;   #_ x      one form, and the discard yields no form
;;   ^m x      two forms (the metadata, then the value); also #^m x
;;   #:ns {}   two forms (the namespace symbol, then the map), and the
;;             map is a collection of its own, so it counts two levels

(def ^:private collection 1)
(def ^:private tag 2)
(def ^:private discard 3)
(def ^:private two-forms 4)
(def ^:private one-form 5)

(defn- whitespace?
  [c]
  (let [c (char c)]
    (or (Character/isWhitespace c) (= c \,))))

(defn- terminating?
  "Whether c ends a token: whitespace or one of EdnReader's terminating
   macro characters. # and ' are constituents."
  [c]
  (or (whitespace? c)
      (case (char c)
        (\" \; \^ \( \) \[ \] \{ \} \\) true
        false)))

(defn- token-end
  "The index after the token that starts at i."
  ^long [^String s ^long i]
  (let [n (.length s)]
    (loop [j i]
      (if (and (< j n) (not (terminating? (.charAt s j))))
        (recur (inc j))
        j))))

(defn- string-end
  "The index after the string literal whose opening quote is at i, or the
   length of s when it is unterminated."
  ^long [^String s ^long i]
  (let [n (.length s)]
    (loop [j (inc i)]
      (if (< j n)
        (case (.charAt s j)
          \\ (recur (+ j 2))
          \" (inc j)
          (recur (inc j)))
        n))))

(defn- line-end
  ^long [^String s ^long i]
  (let [n (.length s)]
    (loop [j i]
      (if (and (< j n) (not= \newline (.charAt s j)) (not= \return (.charAt s j)))
        (recur (inc j))
        j))))

(defn- digit?
  [c]
  (Character/isDigit (char c)))

(defn- number-start?
  "Whether the token at i is a number literal, as EdnReader decides it:
   a digit, or a sign followed by a digit."
  [^String s ^long i]
  (when (< i (.length s))
    (let [c (.charAt s i)]
      (or (digit? c)
          (and (or (= c \+) (= c \-))
               (< (inc i) (.length s))
               (digit? (.charAt s (inc i))))))))

(defn- too-deep!
  [max-depth]
  (refuse! ::too-deep (format "the input nests deeper than %d levels" max-depth) {:limit max-depth}))

(defn- scan!
  "Refuses text that nests deeper than :max-depth, holds a number literal
   longer than :max-number-chars or a tag :readers does not name. Linear
   in the length of the text, and never recursive."
  [^String s {:keys [max-depth max-number-chars readers]}]
  (let [max-depth (long max-depth)
        max-number-chars (long max-number-chars)
        n (.length s)
        stack (int-array (inc (min max-depth n)))
        depth (volatile! 0)
        push! (fn [kind]
                (let [d (long @depth)]
                  (when (>= d max-depth)
                    (too-deep! max-depth))
                  (aset stack d (int kind))
                  (vreset! depth (inc d))))
        ;; A form ended: pop the prefixes it completes.
        complete! (fn []
                    (let [d (long @depth)]
                      (when (pos? d)
                        (let [top (aget stack (dec d))]
                          (cond
                            (= top tag) (do (vreset! depth (dec d)) (recur))
                            (= top discard) (vreset! depth (dec d))
                            (= top two-forms) (aset stack (dec d) (int one-form))
                            (= top one-form) (do (vreset! depth (dec d)) (recur))
                            :else nil)))))
        ;; A closing delimiter: drop the prefixes still waiting for a form
        ;; (the reader refuses those) and the collection it closes.
        close! (fn []
                 (let [d (long @depth)]
                   (when (pos? d)
                     (let [top (aget stack (dec d))]
                       (vreset! depth (dec d))
                       (if (= top collection)
                         (complete!)
                         (recur))))))
        ;; atom! returns the index after the token boxed, so the loop
        ;; below coerces it to keep its index primitive.
        atom! (fn [^long i]
                (let [end (token-end s i)]
                  (when (and (number-start? s i) (> (- end i) max-number-chars))
                    (refuse! ::number-too-long
                             (format "a number literal is longer than %d characters" max-number-chars)
                             {:limit max-number-chars}))
                  (complete!)
                  end))]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            (whitespace? c) (recur (inc i))
            (= c \;) (recur (line-end s i))
            (= c \") (do (complete!) (recur (string-end s i)))
            ;; A character literal: the character after the backslash,
            ;; whatever it is, then the rest of the token.
            (= c \\) (let [end (if (< (inc i) n) (token-end s (+ i 2)) (long n))]
                       (complete!)
                       (recur end))
            (or (= c \() (= c \[) (= c \{)) (do (push! collection) (recur (inc i)))
            (or (= c \)) (= c \]) (= c \})) (do (close!) (recur (inc i)))
            (= c \^) (do (push! two-forms) (recur (inc i)))
            (= c \#)
            (if (< (inc i) n)
              (let [d (.charAt s (inc i))]
                (cond
                  (= d \{) (do (push! collection) (recur (+ i 2)))
                  (= d \_) (do (push! discard) (recur (+ i 2)))
                  (or (= d \^) (= d \:)) (do (push! two-forms) (recur (+ i 2)))
                  (= d \#) (recur (long (atom! (+ i 2))))
                  (Character/isLetter d)
                  (let [end (token-end s (inc i))
                        t (symbol (subs s (inc i) end))]
                    (when-not (contains? readers t)
                      (refuse! ::tag-refused (str "the input holds the tagged literal #" t
                                                  ", and no reader is given for it")
                               {:tag t}))
                    (push! tag)
                    (recur end))
                  ;; The reader refuses any other dispatch character here
                  ;; and reads nothing after it.
                  :else (recur (+ i 2))))
              (recur (inc i)))
            :else (recur (long (atom! i)))))))))

;; The read

(defn- refusing-reader
  [t]
  (fn [_]
    (throw (ex-info (str "the input holds the tagged literal #" t ", and no reader is given for it")
                    {::tag-refused t}))))

(defn- guarded-reader
  [t f]
  (fn [v]
    (try
      (f v)
      (catch Exception e
        (throw (ex-info (str "the reader for #" t " failed: " (or (ex-message e) (str e)))
                        {::tag-failed t}
                        e))))))

(defn- read-options
  [readers]
  ;; :readers shadows the two tags clojure.edn reads on its own; :default
  ;; refuses every other tag. The scan has refused unnamed tags already,
  ;; so these refusals are a second line.
  {:readers (merge {'inst (refusing-reader 'inst) 'uuid (refusing-reader 'uuid)}
                   (into {} (map (fn [[t f]] [t (guarded-reader t f)])) readers))
   :default (fn [t _] ((refusing-reader t) nil))})

(defn- classify!
  [e]
  (let [chain (take-while some? (iterate ex-cause e))
        marked (fn [k] (some #(when (contains? (ex-data %) k) %) chain))]
    (if-let [refused (marked ::tag-refused)]
      (refuse! ::tag-refused (ex-message refused) {:tag (::tag-refused (ex-data refused))} e)
      (if-let [failed (marked ::tag-failed)]
        (refuse! ::tag-failed (ex-message failed) {:tag (::tag-failed (ex-data failed))} e)
        (refuse! ::malformed (str "the input is not valid EDN: " (or (ex-message e) (str e))) {} e)))))

(defn- read-forms
  "Every form of text, read up to the second one when only one is wanted."
  [^String text {:keys [readers one-form? max-depth]}]
  (let [eof (Object.)
        in (PushbackReader. (StringReader. text))
        opts (assoc (read-options readers) :eof eof)]
    (try
      (loop [forms []]
        (let [form (edn/read opts in)]
          (cond
            (identical? form eof) forms
            (and one-form? (= 1 (count forms))) (conj forms form)
            :else (recur (conj forms form)))))
      (catch StackOverflowError e
        ;; The scan keeps nesting this deep away from the reader; a stack
        ;; smaller than :max-depth needs is refused the same way.
        (refuse! ::too-deep (format "the input nests too deep to read within %d levels" max-depth)
                 {:limit max-depth} e))
      (catch Exception e
        (classify! e)))))

(defn- describe
  [x]
  (cond
    (nil? x) "nil"
    (map? x) "a map"
    (vector? x) "a vector"
    (set? x) "a set"
    (seq? x) "a list"
    (string? x) "a string"
    (keyword? x) "a keyword"
    (symbol? x) "a symbol"
    :else (str "a " (.getName (class x)))))

(defn- expect!
  [expect form]
  (when (and expect (not (expect form)))
    (refuse! ::unexpected (str "the input holds " (describe form) ", which is not what was expected")
             {:found (describe form)}))
  form)

(defn- read-text
  [^String text opts]
  (scan! text opts)
  (let [{:keys [one-form? expect]} opts
        forms (read-forms text opts)]
    (if one-form?
      (case (count forms)
        0 (refuse! ::no-form "the input holds no form")
        1 (expect! expect (first forms))
        (refuse! ::trailing-form "the input holds more than one form"))
      (mapv (partial expect! expect) forms))))

;; Entry points

(defn read-string
  "The form EDN text s holds, read within the bounds of opts (see
   `defaults` for the keys), or with :one-form? false a vector of every
   form. Throws an ex-info whose :type is in `refusal-types` otherwise."
  ([s] (read-string s nil))
  ([s opts]
   (let [opts (options opts)
         max-bytes (long (:max-bytes opts))]
     (when-not (string? s)
       (refuse! ::invalid-input (str "the input must be a string, not " (describe s))))
     ;; Every character takes at least one byte, so a string with more
     ;; characters than the limit is too large without encoding it.
     (when (or (> (.length ^String s) max-bytes) (> (utf-8-length s) max-bytes))
       (too-large! max-bytes))
     (read-text s opts))))

(defn read-bytes
  "The form the UTF-8 bytes bs hold, read like `read-string`."
  ([bs] (read-bytes bs nil))
  ([bs opts]
   (let [opts (options opts)]
     (when-not (bytes? bs)
       (refuse! ::invalid-input (str "the input must be a byte array, not " (describe bs))))
     (when (> (count bs) (long (:max-bytes opts)))
       (too-large! (:max-bytes opts)))
     (read-text (decode-utf-8 bs) opts))))

(defn read-stream
  "The form the UTF-8 stream in holds, read like `read-string`. Reads at
   most one byte past :max-bytes, so an endless stream is refused rather
   than buffered, and leaves in open."
  ([in] (read-stream in nil))
  ([in opts]
   (let [opts (options opts)]
     (when-not (instance? InputStream in)
       (refuse! ::invalid-input (str "the input must be an InputStream, not " (describe in))))
     (let [bs (try
                (.readNBytes ^InputStream in (int (inc (long (:max-bytes opts)))))
                (catch IOException e
                  (refuse! ::unreadable (str "the input cannot be read: " (or (ex-message e) (str e))) {} e)))]
       (read-bytes bs opts)))))

(defn read-file
  "The form the UTF-8 file at path holds, read like `read-string`. Refuses
   anything but a regular file, so a FIFO cannot block the read, and adds
   the :file to every refusal."
  ([path] (read-file path nil))
  ([path opts]
   (let [opts (options opts)
         file (str path)]
     (try
       (when-not (fs/regular-file? path)
         (refuse! ::not-a-file (str file " is not a regular file")))
       (with-open [^InputStream in (try
                                     (io/input-stream (fs/file path))
                                     (catch IOException e
                                       (refuse! ::unreadable (str file " cannot be read: " (or (ex-message e) (str e))) {} e)))]
         (read-stream in opts))
       (catch clojure.lang.ExceptionInfo e
         (if (refusal? e)
           (throw (ex-info (str file ": " (ex-message e)) (assoc (ex-data e) :file file) e))
           (throw e)))))))
