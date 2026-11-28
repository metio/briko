;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.inventory
  "The repository inventory: the repositories each source listed when it
   was last fetched, one source per key. aro writes it and krei reads it,
   both through this namespace, so the format has one definition.

   The file is one EDN map from source id (a string) to a vector of
   records, one record per line, sources sorted by id and records by path,
   so a refetch changes only the lines of what changed upstream. Comment
   and blank lines at the top of the file, such as a licence header, are
   the header and survive a rewrite.

   Reading goes through briko.edn: the file is at most :max-bytes long,
   holds no tagged literal and exactly one form, and nests no deeper than
   a valid inventory does. The form must then have the inventory's shape:
   every record holds exactly the keys of `record-keys` that it must and
   may, with values of their types, and no source lists one repository
   twice. Anything else is refused, never dropped, because a reader that
   skips a record it does not understand acts on fewer repositories than
   the inventory names without saying so.

   Every refusal of this namespace is an ex-info whose data carries a
   stable :type, one of `refusal-types`. A refusal of briko.edn (the
   bounds, UTF-8, tags, syntax) or of briko.fs (the write) reaches the
   caller as it is, with its own :type."
  (:require [babashka.fs :as fs]
            [briko.edn :as edn]
            [briko.fs :as bfs]
            [briko.paths :as paths]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io IOException InputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption)
           (java.util Arrays)))

(set! *warn-on-reflection* true)

(def record-keys
  "The keys of a record, in the order a record is written."
  [:host :path :name :namespace :default-branch :archived :fork :topics :properties])

(def ^:private optional-keys #{:properties})

(def ^:private required-keys (set (remove optional-keys record-keys)))

(def defaults
  "The options where the caller gives none.

   - :max-bytes, the largest inventory in UTF-8 bytes: 8 MiB. A record
     takes about 200 bytes, or about 400 with a few custom properties, so
     the limit holds some 20000 to 40000 repositories. `write!` refuses to
     write more than it, so a file written here can always be read back
     with the same limit.
   - :missing, what `read-file` does when no file is at the path: :refuse,
     or :empty for an inventory without sources.
   - :fsync?, for `write!`, whether the new file reaches the disk before
     the call returns: true."
  {:max-bytes 8388608
   :missing :refuse
   :fsync? true})

;; An inventory nests a map of vectors of records, a record holds a map of
;; properties, and a property holds a vector of values: five levels. The
;; limit leaves no room for anything deeper to reach the reader.
(def ^:private max-depth 5)

(def refusal-types
  "Every :type a refusal of this namespace carries."
  #{::invalid-options ::invalid-input ::invalid-shape ::duplicate-id ::missing ::not-a-file
    ::unreadable ::too-large})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

(defn- options
  [opts allowed]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove allowed (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [{:keys [max-bytes missing fsync?] :as opts} (merge defaults opts)]
    (when-not (and (int? max-bytes) (< 0 (long max-bytes) Integer/MAX_VALUE))
      (refuse! ::invalid-options ":max-bytes must be a positive int below Integer/MAX_VALUE" {:option :max-bytes}))
    (when-not (#{:refuse :empty} missing)
      (refuse! ::invalid-options ":missing must be :refuse or :empty" {:option :missing}))
    (when-not (boolean? fsync?)
      (refuse! ::invalid-options ":fsync? must be true or false" {:option :fsync?}))
    opts))

(defn id
  "The id of the repository of a record: its host and path joined by a
   colon, such as \"github.com:metio/aro\"."
  [{:keys [host path]}]
  (str host ":" path))

;; Shape

(defn- non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(defn- strings? [x]
  (and (vector? x) (every? string? x)))

(def ^:private value-checks
  {:host [non-blank-string? "a non-blank string"]
   :path [non-blank-string? "a non-blank string"]
   :name [string? "a string"]
   :namespace [string? "a string"]
   :default-branch [(some-fn nil? string?) "a string or nil"]
   :archived [boolean? "true or false"]
   :fork [boolean? "true or false"]
   :topics [strings? "a vector of strings"]
   :properties [(fn [m] (and (map? m)
                             (every? (fn [[k v]] (and (simple-keyword? k) (or (string? v) (strings? v)))) m)))
                "a map from keyword to a string or a vector of strings"]})

(defn- shape!
  [message data]
  (refuse! ::invalid-shape message data))

(defn- check-record!
  [source index record]
  (let [where {:source source :index index}
        at (str "record " index " of source " (pr-str source))]
    (when-not (map? record)
      (shape! (str at " is not a map") (assoc where :problem :not-a-map)))
    (when-let [unknown (seq (remove (set record-keys) (keys record)))]
      (shape! (str at " holds unknown keys " (pr-str (vec unknown)))
              (assoc where :problem :unknown-keys :keys (vec unknown))))
    (when-let [missing (seq (remove (set (keys record)) (filter required-keys record-keys)))]
      (shape! (str at " lacks the keys " (pr-str (vec missing)))
              (assoc where :problem :missing-keys :keys (vec missing))))
    (doseq [k record-keys
            :when (contains? record k)
            :let [[valid? wanted] (value-checks k)]]
      (when-not (valid? (get record k))
        (shape! (str "the " k " of " at " must be " wanted)
                (assoc where :problem :wrong-type :key k))))
    record))

(defn- check-sources!
  [sources]
  (when-not (map? sources)
    (shape! "the inventory is not a map from source id to records" {:problem :not-a-map}))
  (doseq [[source records] sources]
    (when-not (non-blank-string? source)
      (shape! (str "the source id " (pr-str source) " is not a non-blank string")
              {:problem :source-id :source source}))
    (when-not (vector? records)
      (shape! (str "the records of source " (pr-str source) " are not a vector")
              {:problem :not-a-vector :source source}))
    (doseq [[index record] (map-indexed vector records)]
      (check-record! source index record))
    (when-let [twice (first (for [[rid n] (frequencies (map id records)) :when (< 1 (long n))] rid))]
      (refuse! ::duplicate-id (str "source " (pr-str source) " lists " twice " more than once")
               {:source source :id twice})))
  sources)

(defn- header-line? [line]
  (or (str/blank? line) (str/starts-with? (str/triml line) ";")))

(defn- check-inventory!
  [inventory]
  (when-not (map? inventory)
    (refuse! ::invalid-input "the inventory must be a map of :header and :sources" {}))
  (when-let [unknown (seq (remove #{:header :sources} (keys inventory)))]
    (refuse! ::invalid-input (str "the inventory holds unknown keys " (pr-str (vec unknown))) {:keys (vec unknown)}))
  (let [{:keys [header sources]} inventory]
    (when-not (or (nil? header) (and (string? header) (every? header-line? (str/split-lines header))))
      (refuse! ::invalid-input "the header must be a string of comment and blank lines" {}))
    (check-sources! sources)
    inventory))

;; Reading

(defn- split-header
  "The leading comment and blank lines of text, and the rest."
  [text]
  (let [lines (str/split-lines text)
        header (take-while header-line? lines)]
    [(str/join "\n" header) (str/join "\n" (drop (count header) lines))]))

(defn- edn-options
  [max-bytes one-form?]
  {:max-bytes max-bytes :max-depth max-depth :one-form? one-form?})

(defn- parse*
  "The inventory of text. (read-forms one-form?) reads the input text
   came from through briko.edn within the bounds."
  [text read-forms]
  (let [[header body] (split-header text)]
    {:header header
     :sources (if (str/blank? body)
                ;; Comments and whitespace only: still read, so the bounds
                ;; apply to the whole input.
                (do (read-forms false) {})
                (check-sources! (read-forms true)))}))

(defn parse
  "{:header text :sources {source-id [record]}} from the text of an
   inventory, refusing text that is not one (see the namespace). Options
   :max-bytes (see `defaults`)."
  ([text] (parse text nil))
  ([text opts]
   (let [{:keys [max-bytes]} (options opts #{:max-bytes})]
     (when-not (string? text)
       (refuse! ::invalid-input "the inventory text must be a string" {}))
     (parse* text #(edn/read-string text (edn-options max-bytes %))))))

(defn- read-bounded
  "At most max-bytes + 1 bytes of the regular file at path."
  ^bytes [path max-bytes]
  (try
    (with-open [^InputStream in (io/input-stream (fs/file path))]
      (.readNBytes in (int (inc (long max-bytes)))))
    (catch IOException e
      (refuse! ::unreadable (str "the inventory cannot be read: " (or (ex-message e) (str e))) {} e))))

(defn read-file
  "The inventory in the file at path, read like `parse`, with :file added
   to every refusal. Without a file at path, refuses with ::missing, or
   returns {:header \"\" :sources {}} with :missing :empty. A symbolic link
   is followed; one that leads nowhere, and anything but a regular file,
   is refused with ::not-a-file. Options :max-bytes and :missing (see
   `defaults`)."
  ([path] (read-file path nil))
  ([path opts]
   (let [{:keys [max-bytes missing]} (options opts #{:max-bytes :missing})]
     (when-not (or (non-blank-string? path) (instance? java.nio.file.Path path) (instance? java.io.File path))
       (refuse! ::invalid-input "the path must be a non-blank string, a Path or a File" {}))
     (let [file (str path)]
       (try
         (cond
           (not (fs/exists? path {:nofollow-links true}))
           (if (= :empty missing)
             {:header "" :sources {}}
             (refuse! ::missing (str "no inventory at " file) {}))

           (not (fs/regular-file? path))
           (refuse! ::not-a-file (str file " is not a regular file") {})

           :else
           (let [bs (read-bounded path max-bytes)]
             (when (< (long max-bytes) (alength bs))
               (refuse! ::too-large (format "the inventory is larger than %d bytes" max-bytes) {:limit max-bytes}))
             ;; read-bytes refuses invalid UTF-8 before anything else, so
             ;; the header is split from text that decodes cleanly.
             (let [text (String. bs StandardCharsets/UTF_8)]
               (parse* text #(edn/read-bytes bs (edn-options max-bytes %))))))
         (catch clojure.lang.ExceptionInfo e
           (if (or (refusal? e) (edn/refusal? e))
             (throw (ex-info (str file ": " (ex-message e)) (assoc (ex-data e) :file file) e))
             (throw e))))))))

;; Writing

(defn- render-record
  [record]
  ;; a map read back from the file is a hash map above eight entries and
  ;; prints in hash order, so maps are sorted to render the same either way
  (str "{" (str/join " " (for [k record-keys :when (contains? record k)
                               :let [v (get record k)]]
                           (str k " " (pr-str (if (map? v) (into (sorted-map) v) v)))))
       "}"))

(defn render
  "The text of an inventory: the header, then sources sorted by id and
   records by path, one per line. Refuses an inventory `parse` would
   refuse to read back."
  [inventory]
  (let [{:keys [header sources]} (check-inventory! inventory)
        body (for [[source records] (sort-by key sources)]
               (str (pr-str source) "\n ["
                    (str/join "\n  " (map render-record (sort-by :path records)))
                    "]"))]
    (binding [*print-length* nil
              *print-level* nil
              *print-meta* false
              *print-namespace-maps* false]
      (str (when-not (str/blank? header) (str (str/trimr header) "\n\n"))
           "{" (str/join "\n " (doall body)) "}\n"))))

(defn- same-content?
  "Whether the regular file at target holds exactly bs, reading no more
   than bs is long."
  [target ^bytes bs]
  (and (Files/isRegularFile target (make-array LinkOption 0))
       (= (alength bs) (Files/size target))
       (Arrays/equals bs (read-bounded target (alength bs)))))

(defn write!
  "Writes inventory to path through briko.fs/atomic-write! and returns
   whether the content changed. A file that already holds the same text is
   left alone, and missing parent directories are created. Refuses an
   inventory `render` refuses, and with ::too-large one whose text is
   larger than :max-bytes. Options :max-bytes and :fsync? (see
   `defaults`)."
  ([path inventory] (write! path inventory nil))
  ([path inventory opts]
   (let [{:keys [max-bytes fsync?]} (options opts #{:max-bytes :fsync?})
         _ (when-not (or (non-blank-string? path) (instance? java.nio.file.Path path) (instance? java.io.File path))
             (refuse! ::invalid-input "the path must be a non-blank string, a Path or a File" {}))
         target (fs/absolutize (fs/path path))
         bs (.getBytes ^String (render inventory) StandardCharsets/UTF_8)]
     (when (< (long max-bytes) (alength bs))
       (refuse! ::too-large (format "the inventory is larger than %d bytes" max-bytes)
                {:limit max-bytes :file (str target)}))
     (if (same-content? target bs)
       false
       (do (fs/create-dirs (fs/parent target))
           (bfs/atomic-write! target bs {:fsync? fsync?})
           true)))))

(defn default-path
  "Where the inventory lives unless configured otherwise:
   inventory.edn in the aro directory of the XDG state home. opts are
   briko.paths's environment options, {:getenv f :getprop f}."
  ([] (default-path nil))
  ([opts] (str (fs/path (paths/state-home opts) "aro" "inventory.edn"))))
