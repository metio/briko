;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json
  "Reads JSON from text that may come from anywhere, strictly and within
   bounds the caller sets, and writes JSON with every hidden code point
   escaped.

   A read accepts exactly the JSON of RFC 8259 and refuses:

     - input longer than :max-bytes in UTF-8, or not valid UTF-8;
     - nesting deeper than :max-depth arrays and objects, refused by the
       streaming tokenizer before the recursive builder above it gets
       that deep;
     - a number literal longer than :max-number-chars characters, sign,
       point and exponent included, because a long literal costs time to
       convert, and a decimal whose value overflows a double, because
       Infinity has no JSON spelling;
     - an object that names one key twice, because two readers that keep
       different duplicates read different documents;
     - anything after the one value, and no value at all, unless
       :one-value? false asks for every value of the text;
     - comments, single quotes, trailing commas, NaN, leading zeros and
       raw control characters inside strings.

   A top-level array is read whole before the call returns, so a
   truncated document fails inside it rather than wherever a lazy element
   is first reached. Keys stay strings unless :keys asks otherwise.
   Decimals are doubles and integers are longs or BigIntegers, as cheshire
   reads them; babashka's cheshire offers no BigDecimal reading. An
   escaped lone surrogate (\"\\ud800\") reads in a value; in an object key
   jackson-core 2.23 refuses it while the Jackson in babashka v1.13.229
   reads it.

   `write-string` prints with cheshire and then writes every hidden code
   point (see `hidden?`) as \\uXXXX, so text that reaches a terminal or
   an agent carries no control, bidirectional or zero-width character
   raw, while readable non-ASCII text such as Größe stays as it is. It
   refuses a map whose keys print to the same name.

   Every refusal is an ex-info whose data carries a stable :type, one of
   `refusal-types`. No refusal carries the input or a piece of it: a
   position is given as :line and :column instead.

   JSON is read and written with cheshire, resolved when first used, so
   the namespace loads on the JVM without the dependency; babashka
   carries cheshire, a JVM consumer declares it, and a native image must
   require `cheshire.core` and `cheshire.factory` where it is built."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io IOException InputStream StringReader)
           (java.nio ByteBuffer CharBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)
           (java.util.regex Matcher)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a read uses where the caller gives none.

   - :max-bytes, the longest input in UTF-8 bytes: 1 MiB.
   - :max-depth, the deepest nesting of arrays and objects: 64.
   - :max-number-chars, the longest number literal: 100 characters.
   - :keys, how object keys arrive: :string (as written), :keyword
     (every key a keyword), a set of strings (those keys as keywords, all
     others strings) or a function of the key string.
   - :allow-duplicate-keys?, whether an object may name a key twice, the
     last one winning: false.
   - :one-value?, whether the text holds exactly one value: true. With
     false a read returns a vector of every value, as of JSON Lines.
   - :expect, a predicate every value must satisfy: none."
  {:max-bytes 1048576
   :max-depth 64
   :max-number-chars 100
   :keys :string
   :allow-duplicate-keys? false
   :one-value? true
   :expect nil})

(def write-defaults
  "The options a write uses where the caller gives none.

   - :pretty?, whether to indent the output: false.
   - :allow-duplicate-keys?, whether a map may hold keys that print to the
     same name, such as :a and \"a\": false.
   - :max-depth, the deepest nesting of maps and collections: 256, well
     within the stack cheshire's recursive printer needs."
  {:pretty? false
   :allow-duplicate-keys? false
   :max-depth 256})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-input ::too-large ::invalid-utf8 ::too-deep
    ::number-too-long ::number-out-of-range ::duplicate-key ::malformed
    ::no-value ::trailing-value ::unexpected ::key-failed ::not-a-file
    ::unreadable ::not-encodable})

(defn- refuse!
  ([type message] (refuse! type message {} nil))
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

;; cheshire

(defn- cheshire-fn [sym] (requiring-resolve sym))

(defn- factory-var ^clojure.lang.Var [] (requiring-resolve 'cheshire.factory/*json-factory*))

;; Options

(defn- pos-int-below?
  [limit x]
  (and (int? x) (pos? (long x)) (< (long x) (long limit))))

(defn- keys-option?
  [k]
  (or (contains? #{:string :keyword} k)
      (and (set? k) (every? string? k))
      (fn? k)))

(def ^:private option-checks
  ;; :max-bytes stays below Integer/MAX_VALUE because the stream read asks
  ;; for one byte more than the limit in an int.
  {:max-bytes [(partial pos-int-below? Integer/MAX_VALUE) "a positive int below Integer/MAX_VALUE"]
   :max-depth [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   :max-number-chars [(partial pos-int-below? Integer/MAX_VALUE) "a positive int"]
   :keys [keys-option? ":string, :keyword, a set of strings or a function"]
   :allow-duplicate-keys? [boolean? "a boolean"]
   :one-value? [boolean? "a boolean"]
   :expect [(fn [p] (or (nil? p) (ifn? p))) "nil or a predicate"]
   :pretty? [boolean? "a boolean"]})

(defn- options
  [opts known]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {:options opts}))
  (when-let [unknown (seq (remove (set (keys known)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge known opts)]
    (doseq [k (keys known)
            :let [[valid? wanted] (option-checks k)]]
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

;; Numbers
;;
;; Jackson bounds the parts of a number separately, so its limit alone
;; would let a literal of several times :max-number-chars through. A
;; native pre-check finds whether the text holds a run of number
;; characters as long as the limit, or an exponent of three digits, the
;; only ways a literal within 300 characters can overflow a double; only
;; then does a token scan skip the strings and judge each number literal.

(def ^:private number-token
  ;; A string literal (the loop unrolled with possessive quantifiers, so a
  ;; long string neither backtracks nor deepens the regex stack), a quote
  ;; that opens a string never closed, or a number literal.
  #"(\"[^\"\\]*+(?:\\[\s\S][^\"\\]*+)*+\")|(\")|(-?[0-9][-+.eE0-9]*)")

(defn- number-suspect
  [^long max-number-chars]
  (re-pattern (str "[eE][-+]?[0-9]{3}|[-+.eE0-9]{" (min (inc max-number-chars) 300) ",}")))

(defn- check-numbers!
  [^String s {:keys [max-number-chars]}]
  (let [max-number-chars (long max-number-chars)]
    (when (re-find (number-suspect max-number-chars) s)
      (let [^Matcher m (re-matcher number-token s)]
        (while (and (.find m) (nil? (.group m 2)))
          (when-let [^String n (.group m 3)]
            (when (> (.length n) max-number-chars)
              (refuse! ::number-too-long
                       (format "a number literal is longer than %d characters" max-number-chars)
                       {:limit max-number-chars}))
            (when (and (re-find #"[.eE]" n)
                       (try (Double/isInfinite (Double/parseDouble n))
                            (catch NumberFormatException _ false)))
              (refuse! ::number-out-of-range "a number literal is too large for a double"))))))))

;; The read

(defn- position
  "The :line and :column Jackson reports in message, which names no
   content of the input since its source is redacted."
  [message]
  (if-let [[_ line column] (re-find #"line: (\d+), column: (\d+)" (str message))]
    {:line (parse-long line) :column (parse-long column)}
    {}))

(defn- at
  [{:keys [line column]}]
  (if line (format " at line %d, column %d" line column) ""))

(defn- classify!
  "Refuses after cheshire or Jackson threw e while reading. The message
   and the cause are left out, because Jackson's message quotes the
   token it stopped at, which may be a secret."
  [^Throwable e {:keys [max-depth max-number-chars max-bytes]}]
  (let [chain (take-while some? (iterate ex-cause e))
        message (str (ex-message e))
        pos (position message)]
    (if-let [failed (some #(when (contains? (ex-data %) ::key-failed) %) chain)]
      (refuse! ::key-failed (str "the :keys function failed" (at pos)) pos (ex-cause failed))
      (let [class-name (.getName (class e))]
        (cond
          (str/ends-with? class-name "StreamConstraintsException")
          (cond
            (str/includes? message "nesting depth")
            (refuse! ::too-deep (format "the input nests deeper than %d levels" max-depth)
                     (assoc pos :limit max-depth))
            (str/includes? message "Number value length")
            (refuse! ::number-too-long (format "a number literal is longer than %d characters" max-number-chars)
                     (assoc pos :limit max-number-chars))
            :else
            (too-large! max-bytes))

          (str/starts-with? message "Duplicate field")
          (refuse! ::duplicate-key (str "the input names an object key twice" (at pos)) pos)

          :else
          (refuse! ::malformed (str "the input is not valid JSON" (at pos)) pos))))))

(defn- key-fn
  [k]
  (cond
    (= k :string) false
    (= k :keyword) true
    (set? k) (fn [s] (if (contains? k s) (keyword s) s))
    :else (fn [s]
            (try
              (k s)
              (catch Exception e
                (throw (ex-info "the :keys function failed" {::key-failed true} e)))))))

(defn- factory
  [{:keys [max-depth max-number-chars max-bytes allow-duplicate-keys?]}]
  ((cheshire-fn 'cheshire.factory/make-json-factory)
   {:strict-duplicate-detection (not allow-duplicate-keys?)
    :allow-unquoted-control-chars false
    :allow-comments false
    :allow-single-quotes false
    :allow-unquoted-field-names false
    :allow-non-numeric-numbers false
    :allow-numeric-leading-zeros false
    :allow-trailing-comma false
    :allow-backslash-escaping false
    :max-input-nesting-depth max-depth
    :max-input-number-length max-number-chars
    ;; Strings and names are bounded by :max-bytes, so Jackson's own
    ;; limits on them never refuse first.
    :max-input-string-length max-bytes
    :max-input-name-length max-bytes}))

(defn- describe
  [x]
  (cond
    (nil? x) "null"
    (map? x) "an object"
    (vector? x) "an array"
    (string? x) "a string"
    (boolean? x) "a boolean"
    (number? x) "a number"
    :else (str "a " (.getName (class x)))))

(defn- read-values
  "Every value of text, read up to the second one when only one is wanted."
  [^String text {:keys [one-value? max-depth] :as opts}]
  (let [parsed-seq (cheshire-fn 'cheshire.core/parsed-seq)
        k (key-fn (:keys opts))]
    (with-bindings {(factory-var) (factory opts)}
      (try
        (let [values (parsed-seq (StringReader. text) k)]
          (if one-value?
            (let [first-value (vec (take 1 values))]
              ;; Asking for a second value makes the parser read past the
              ;; first, so text after it is refused rather than dropped,
              ;; whether it parses or not.
              (when (and (seq first-value) (try (seq (rest values)) (catch Exception _ true)))
                (refuse! ::trailing-value "the input holds text after its JSON value"))
              first-value)
            (vec values)))
        (catch StackOverflowError e
          (refuse! ::too-deep (format "the input nests too deep to read within %d levels" max-depth)
                   {:limit max-depth} e))
        (catch clojure.lang.ExceptionInfo e
          (if (refusal? e) (throw e) (classify! e opts)))
        (catch Exception e (classify! e opts))))))

(defn- expect!
  [expect value]
  (when (and expect (not (expect value)))
    (refuse! ::unexpected (str "the input holds " (describe value) ", which is not what was expected")
             {:found (describe value)}))
  value)

(defn- read-text
  [^String text {:keys [one-value? expect] :as opts}]
  (check-numbers! text opts)
  (let [values (read-values text opts)]
    (if one-value?
      (if (seq values)
        (expect! expect (first values))
        (refuse! ::no-value "the input holds no JSON value"))
      (mapv (partial expect! expect) values))))

;; Entry points

(defn parse-string
  "The JSON value text s holds, read within the bounds of opts (see
   `defaults` for the keys), or with :one-value? false a vector of every
   value. Throws an ex-info whose :type is in
   `refusal-types` otherwise."
  ([s] (parse-string s nil))
  ([s opts]
   (let [opts (options opts defaults)
         max-bytes (long (:max-bytes opts))]
     (when-not (string? s)
       (refuse! ::invalid-input (str "the input must be a string, not " (describe s))))
     ;; Every character takes at least one byte, so a string with more
     ;; characters than the limit is too large without encoding it.
     (when (or (> (.length ^String s) max-bytes) (> (utf-8-length s) max-bytes))
       (too-large! max-bytes))
     (read-text s opts))))

(defn parse-bytes
  "The JSON value the UTF-8 bytes bs hold, read like `parse-string`."
  ([bs] (parse-bytes bs nil))
  ([bs opts]
   (let [opts (options opts defaults)]
     (when-not (bytes? bs)
       (refuse! ::invalid-input (str "the input must be a byte array, not " (describe bs))))
     (when (> (count bs) (long (:max-bytes opts)))
       (too-large! (:max-bytes opts)))
     (read-text (decode-utf-8 bs) opts))))

(defn parse-stream
  "The JSON value the UTF-8 stream in holds, read like `parse-string`.
   Reads at most one byte past :max-bytes, so an endless stream is
   refused rather than buffered, and leaves in open."
  ([in] (parse-stream in nil))
  ([in opts]
   (let [opts (options opts defaults)]
     (when-not (instance? InputStream in)
       (refuse! ::invalid-input (str "the input must be an InputStream, not " (describe in))))
     (let [bs (try
                (.readNBytes ^InputStream in (int (inc (long (:max-bytes opts)))))
                (catch IOException e
                  (refuse! ::unreadable (str "the input cannot be read: " (or (ex-message e) (str e))) {} e)))]
       (parse-bytes bs opts)))))

(defn parse-file
  "The JSON value the UTF-8 file at path holds, read like `parse-string`.
   Refuses anything but a regular file, so a FIFO cannot block the read,
   and adds the :file to every refusal."
  ([path] (parse-file path nil))
  ([path opts]
   (let [opts (options opts defaults)
         file (str path)]
     (try
       (when-not (fs/regular-file? path)
         (refuse! ::not-a-file (str file " is not a regular file")))
       (with-open [^InputStream in (try
                                     (io/input-stream (fs/file path))
                                     (catch IOException e
                                       (refuse! ::unreadable (str file " cannot be read: " (or (ex-message e) (str e))) {} e)))]
         (parse-stream in opts))
       (catch clojure.lang.ExceptionInfo e
         (if (refusal? e)
           (throw (ex-info (str file ": " (ex-message e)) (assoc (ex-data e) :file file) e))
           (throw e)))))))

;; Hidden code points

(def ^:private hidden-class
  "The code points that must not reach a reader raw, as a regex character
   class: control characters (C0, DEL, C1), format characters (bidirectional
   controls, zero-width characters, the byte order mark, tags), line and
   paragraph separators, surrogates standing alone, unassigned code points and
   noncharacters, and the default-ignorable code points of no such category
   (the combining grapheme joiner, the Hangul fillers, the Khmer inherent
   vowels, the Mongolian free variation selectors and the variation
   selectors). Each is invisible, reorders text, moves a terminal's cursor or
   cannot be encoded. A regex is matched natively, so a large text is
   scanned in one pass of the JVM's own code. Which code points are
   unassigned depends on the Unicode version of the running JVM."
  (str "\\p{Cc}\\p{Cf}\\p{Zl}\\p{Zp}\\p{Cs}\\p{Cn}"
       "\\x{034F}\\x{115F}-\\x{1160}\\x{17B4}-\\x{17B5}\\x{180B}-\\x{180F}"
       "\\x{3164}\\x{FE00}-\\x{FE0F}\\x{FFA0}\\x{E0100}-\\x{E01EF}"))

(def ^:private hidden-pattern
  (re-pattern (str "[" hidden-class "]")))

(def ^:private hidden-in-text
  "A hidden code point other than tab, newline and carriage return: the
   whitespace JSON reads between values, and which a JSON printer already
   escapes inside a string."
  (re-pattern (str "[" hidden-class "&&[^\\t\\n\\r]]")))

(defn hidden?
  "Whether the code point cp must not reach a reader raw: a control,
   format, separator, lone surrogate, unassigned or other
   default-ignorable code point."
  [cp]
  (boolean (re-matches hidden-pattern (String. (Character/toChars (int cp))))))

(defn- unicode-escape
  "The \\uXXXX escape of the code points in s, a surrogate pair for one
   above U+FFFF, which a JSON string reads back as the same characters."
  [^String s]
  (str/join (map #(format "\\u%04X" (int %)) s)))

(defn escape-hidden
  "printed, a text cheshire (or any JSON writer) produced, with every
   hidden code point (see `hidden?`) written as \\uXXXX. Outside strings a
   JSON writer puts nothing but ASCII and whitespace, and inside them it
   already escapes tab, newline and carriage return, so every other
   hidden code point sits in a string, where the escape reads back as the
   same character. Other non-ASCII text stays readable."
  [printed]
  (let [printed (str printed)]
    ;; Printable ASCII, tab, newline and return hold no hidden code point,
    ;; and this class is matched far faster than the Unicode one.
    (if (and (re-find #"[^\x20-\x7E\t\n\r]" printed)
             (re-find hidden-in-text printed))
      (str/replace printed hidden-in-text unicode-escape)
      printed)))

;; The write

(defn- printed-name
  "The name cheshire prints for the map key k."
  [k]
  (if (keyword? k) (subs (str k) 1) (str k)))

(defn- check-value!
  "Refuses x when it nests deeper than max-depth maps and collections or,
   unless duplicates are allowed, holds a map with two keys that print to
   the same name. Iterative, so a value too deep for cheshire's recursive
   printer is refused before the printer sees it."
  [x max-depth allow-duplicate-keys?]
  (let [max-depth (long max-depth)]
    (loop [todo (list [x 0])]
      (when-let [[[v depth] & more] (seq todo)]
        (let [depth (long depth)
              nested? (or (instance? java.util.Map v) (instance? java.util.Collection v) (seq? v))]
          (when (and nested? (>= depth max-depth))
            (refuse! ::too-deep (format "the value nests deeper than %d levels" max-depth) {:limit max-depth}))
          (cond
            (instance? java.util.Map v)
            (let [m ^java.util.Map v]
              (when-not (or allow-duplicate-keys?
                            (= (.size m) (count (into #{} (map printed-name) (.keySet m)))))
                (refuse! ::duplicate-key "a map holds two keys that print to the same JSON name"))
              (recur (into more (map #(vector % (inc depth))) (.values m))))

            nested?
            (recur (into more (map #(vector % (inc depth))) v))

            :else (recur more)))))))

(defn write-string
  "x as JSON text, printed by cheshire with every hidden code point
   escaped (see `escape-hidden`), within the options of `write-defaults`.
   Refuses a value nested deeper than :max-depth, a value cheshire cannot
   encode and, unless allowed, a map whose keys print to the same name."
  ([x] (write-string x nil))
  ([x opts]
   (let [{:keys [pretty? allow-duplicate-keys? max-depth]} (options opts write-defaults)]
     (check-value! x max-depth allow-duplicate-keys?)
     (escape-hidden
      (with-bindings {(factory-var) ((cheshire-fn 'cheshire.factory/make-json-factory) {})}
        (try
          ((cheshire-fn 'cheshire.core/generate-string) x {:pretty pretty?})
          (catch StackOverflowError e
            ;; A nesting the walk does not see, such as Java arrays.
            (refuse! ::too-deep "the value nests too deep to print" {} e))
          (catch Exception e
            ;; cheshire's message prints the value it could not encode.
            (let [class-name (.getName (class e))]
              (if (and (str/ends-with? class-name "StreamConstraintsException")
                       (str/includes? (str (ex-message e)) "nesting depth"))
                (refuse! ::too-deep "the value nests deeper than the JSON writer allows")
                (refuse! ::not-encodable "the value holds something JSON cannot encode"))))))))))
