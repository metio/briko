;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.skill-manifest
  "The signed file manifest of an agent skill served through the MCP
   Skills extension (`io.modelcontextprotocol/skills`), format
   `skill-manifest-v1`.

   A skill entry names its SKILL.md as `uri` and lists every file of the
   skill under `resources`, each with its `uri`, `digest` (`sha256:` and
   64 lowercase hex digits) and `size`. The extension's digests only say
   the bytes match the manifest; the signature says which actor vouches
   for the manifest. It is an SSHSIG over the canonical bytes

       skill-manifest v1\\n
       skill <uri of SKILL.md>\\n
       file <digest> <size> <uri>\\n   one per file, sorted by URI

   and travels in the entry's `_meta` under `meta-key`, as
   `{\"format\" \"skill-manifest-v1\" \"signed\" true \"namespace\" …
   \"actor\" … \"signature\" …}`, or as `{… \"signed\" false \"reason\" …}`
   when the server holds no signature.

   An entry is accepted only when the bytes cannot be ambiguous and name
   one skill: every URI is `skill://<name>/<path>` with a skill name of
   lowercase letters, digits and inner hyphens and path segments of
   `A-Z a-z 0-9 . _ -` that are neither `.` nor `..`; the entry's `uri`
   is `skill://<name>/SKILL.md` and is among the files; every file
   belongs to that skill; no URI repeats; sizes are non-negative
   integers within the caller's limits.

   The limits and the signature namespace are arguments of every call,
   so no product's ceiling or namespace becomes another's by default:
   limits are `{:max-files n :max-file-bytes n :max-skill-bytes n}`,
   checked against the sizes the manifest declares, and a namespace
   matches `namespace-pattern` (`<product>-skill-manifest`)."
  (:require [briko.ssh-sign :as ssh-sign]
            [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util Base64 HexFormat)))

(set! *warn-on-reflection* true)

(def manifest-format
  "The `format` of the `_meta` value; the canonical bytes open with
   `skill-manifest v1`."
  "skill-manifest-v1")

(def meta-key
  "The `_meta` key of a skill entry that carries the manifest signature."
  "wtf.metio/skill-manifest")

(def namespace-pattern
  "The SSHSIG namespaces of manifest signatures, one per product, so a
   manifest signature never passes as any other signature of the same
   key."
  #"[a-z][a-z0-9]*(-[a-z0-9]+)*-skill-manifest")

(def skill-file "SKILL.md")

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid-arguments})))

;; URIs

(def ^:private skill-name-re #"[a-z0-9]+(-[a-z0-9]+)*")
(def ^:private segment-re #"[A-Za-z0-9._-]+")

(defn- valid-segment? [s]
  (and (string? s) (re-matches segment-re s) (not (#{"." ".."} s))))

(defn valid-skill-name?
  "Whether `n` is a skill name: lowercase letters and digits, single
   inner hyphens."
  [n]
  (boolean (and (string? n) (re-matches skill-name-re n))))

(defn valid-relative-path?
  "Whether `p` is a path inside a skill: non-empty segments of
   `A-Z a-z 0-9 . _ -`, none of them `.` or `..`, no leading or trailing
   slash."
  [p]
  (and (string? p) (not (str/blank? p))
       (every? valid-segment? (str/split p #"/" -1))))

(defn parse-uri
  "`skill://<name>/<path>` as `[name path]`, or nil for anything else: a
   foreign scheme, a traversal segment, an absolute or empty path, an
   escaped character."
  [uri]
  (when (and (string? uri) (str/starts-with? uri "skill://"))
    (let [rest (subs uri (count "skill://"))
          i (str/index-of rest "/")]
      (when (and i (pos? (long i)))
        (let [skill (subs rest 0 i)
              path (subs rest (inc (long i)))]
          (when (and (valid-skill-name? skill) (valid-relative-path? path))
            [skill path]))))))

(defn skill-uri [skill path] (str "skill://" skill "/" path))

(defn digest
  "`sha256:` and the lowercase hex sha256 of the byte array `b`."
  [^bytes b]
  (str "sha256:" (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") b))))

;; Entries

(defn- field
  "The value of field k (a keyword) of map m, read by keyword or by
   string, as JSON parses it; ::ambiguous when m holds both."
  [m k]
  (let [kw? (contains? m k)
        s? (contains? m (name k))]
    (cond
      (and kw? s?) ::ambiguous
      kw? (get m k)
      :else (get m (name k)))))

(defn- meta-of [entry]
  (let [m (field entry :_meta)]
    (when (map? m)
      (let [kw (keyword meta-key)]
        (if (and (contains? m kw) (contains? m meta-key))
          ::ambiguous
          (get m meta-key (get m kw)))))))

(defn- check-limits!
  [{:keys [max-files max-file-bytes max-skill-bytes] :as limits}]
  (when-not (and (map? limits)
                 (every? #(and (integer? %) (pos? (long %))) [max-files max-file-bytes max-skill-bytes]))
    (invalid! "limits must give :max-files, :max-file-bytes and :max-skill-bytes as positive integers")))

(def ^:private digest-re #"sha256:[0-9a-f]{64}")

(defn problem
  "Why the entry (keyword or string keys) cannot be signed or verified
   under `limits`, as a string, or nil when it can."
  [entry limits]
  (check-limits! limits)
  (let [uri (when (map? entry) (field entry :uri))
        resources (when (map? entry) (field entry :resources))
        [skill path] (parse-uri uri)
        files (when (sequential? resources)
                (mapv #(if (map? %) {:uri (field % :uri) :digest (field % :digest) :size (field % :size)} %)
                      resources))]
    (cond
      (not (map? entry)) "not a skill entry"
      (nil? skill) (str "the skill URI " (pr-str uri) " is not skill://<name>/<path>")
      (not= skill-file path) (str "the skill URI " uri " does not name the skill's " skill-file)
      (not (sequential? resources)) "the entry has no list of resources"
      (empty? files) "the entry lists no files"
      (not (every? map? files)) "a resource is not an object"
      :else
      (or (some (fn [{u :uri d :digest s :size}]
                  (cond
                    (nil? (parse-uri u)) (str "the resource URI " (pr-str u) " is not skill://<name>/<path>")
                    (not= skill (first (parse-uri u))) (str "the resource " u " belongs to another skill")
                    (not (and (string? d) (re-matches digest-re d))) (str "the resource " u " has no sha256 digest")
                    (not (and (integer? s) (<= 0 s))) (str "the resource " u " has no size")
                    (< (:max-file-bytes limits) s)
                    (str "the resource " u " exceeds " (:max-file-bytes limits) " bytes")))
                files)
          (cond
            (not (apply distinct? (map :uri files))) "the manifest names a file twice"
            (not-any? #(= uri (:uri %)) files) (str "the manifest does not list " uri)
            (< (long (:max-files limits)) (count files))
            (str "the skill exceeds " (:max-files limits) " files")
            (< (:max-skill-bytes limits) (reduce + (map :size files)))
            (str "the skill exceeds " (:max-skill-bytes limits) " bytes"))))))

(defn canonical-bytes
  "The bytes a manifest signature covers. Throws an ex-info with :type
   ::malformed and the :reason when `problem` refuses the entry."
  ^bytes [entry limits]
  (when-let [reason (problem entry limits)]
    (throw (ex-info (str "the skill manifest cannot be encoded: " reason) {:type ::malformed :reason reason})))
  (let [files (for [r (field entry :resources)]
                {:uri (field r :uri) :digest (field r :digest) :size (field r :size)})]
    (.getBytes (str "skill-manifest v1\n"
                    "skill " (field entry :uri) "\n"
                    (str/join (for [{u :uri d :digest s :size} (sort-by :uri files)]
                                (str "file " d " " s " " u "\n"))))
               StandardCharsets/UTF_8)))

;; Signing

(defn- check-namespace! [namespace]
  (when-not (and (string? namespace) (re-matches namespace-pattern namespace))
    (invalid! (str "not a skill-manifest namespace: " (pr-str namespace)))))

(defn unsigned
  "The `_meta` value of an entry the server holds no signature for."
  [reason]
  {"format" manifest-format "signed" false "reason" (str reason)})

(defn sign
  "The signed `_meta` value of `entry`: the canonical bytes signed with
   `key-file` (see briko.ssh-sign/sign-bytes) as `actor` in `namespace`.
   `opts` carries :limits (required) and the briko.ssh-sign options under
   :ssh-sign. Throws when the entry is malformed or signing fails; the
   caller decides what an unsigned entry says."
  [entry {:keys [key-file actor namespace limits ssh-sign]}]
  (check-namespace! namespace)
  (when-not (and (string? actor) (not (str/blank? actor)))
    (invalid! "a manifest signature needs an actor"))
  (let [message (canonical-bytes entry limits)
        sig (ssh-sign/sign-bytes key-file message namespace ssh-sign)]
    {"format" manifest-format "signed" true "namespace" namespace "actor" actor
     "signature" (String. sig StandardCharsets/UTF_8)}))

;; Verifying

(defn verify
  "The verdict on the manifest signature of `entry` (keyword or string
   keys) under the allowed-signers text `allowed-signers`. `opts`:
   :namespaces, the set of namespaces accepted (required, each matching
   namespace-pattern); :limits (required); :ssh-sign, briko.ssh-sign
   options.

     {:verdict :verified :actor a :namespace ns}
     {:verdict :missing}                  no manifest value in _meta
     {:verdict :malformed :reason r}      the manifest or the value cannot be read
     {:verdict :unsigned :reason r}       the server says it holds no signature
     {:verdict :wrong-namespace :namespace ns}
     {:verdict :bad-signature :actor a}   the signature does not verify for its actor
     {:verdict :unchecked :actor a}       no reader could judge the signature

   Only :verified passes."
  [allowed-signers entry {:keys [namespaces limits ssh-sign]}]
  (when-not (string? allowed-signers)
    (invalid! "the allowed signers must be a string"))
  (when-not (and (set? namespaces) (seq namespaces))
    (invalid! ":namespaces must be a non-empty set"))
  (run! check-namespace! namespaces)
  (check-limits! limits)
  (let [m (when (map? entry) (meta-of entry))
        reason (problem entry limits)
        marker (when (map? m) (into {} (map (fn [k] [k (field m k)])) [:format :signed :namespace :actor :signature :reason]))
        {fmt :format signed :signed namespace :namespace actor :actor signature :signature} marker]
    (cond
      (not (map? entry)) {:verdict :malformed :reason "not a skill entry"}
      (= ::ambiguous (field entry :_meta)) {:verdict :malformed :reason "_meta is given twice"}
      (= ::ambiguous m) {:verdict :malformed :reason "the manifest value is given twice"}
      (nil? m) {:verdict :missing}
      (not (map? m)) {:verdict :malformed :reason "the manifest value is not an object"}
      (some #{::ambiguous} (vals marker)) {:verdict :malformed :reason "the manifest value gives a field twice"}
      (not= manifest-format fmt) {:verdict :malformed :reason (str "unknown manifest format " (pr-str fmt))}
      reason {:verdict :malformed :reason reason}
      (false? signed) {:verdict :unsigned :reason (str (:reason marker))}
      (not (true? signed)) {:verdict :malformed :reason "signed is neither true nor false"}
      (not (contains? namespaces namespace)) {:verdict :wrong-namespace :namespace namespace}
      (not (and (string? actor) (not (str/blank? actor)) (string? signature)))
      {:verdict :malformed :reason "a signed value needs an actor and a signature"}
      :else
      (let [ok (ssh-sign/verify-text allowed-signers (canonical-bytes entry limits)
                                     (.getBytes ^String signature StandardCharsets/UTF_8)
                                     actor namespace ssh-sign)]
        (case ok
          true {:verdict :verified :actor actor :namespace namespace}
          false {:verdict :bad-signature :actor actor}
          {:verdict :unchecked :actor actor})))))

(defn check-file
  "nil when the byte array `b` matches the manifest line for `uri` in
   `entry`, else the reason."
  [entry uri ^bytes b]
  (let [resources (field entry :resources)
        line (when (sequential? resources)
               (some #(when (and (map? %) (= uri (field % :uri))) %) resources))]
    (cond
      (nil? line) (str uri " is not in the manifest")
      (not= (field line :size) (alength b))
      (str uri ": " (alength b) " bytes, the manifest says " (field line :size))
      (not= (field line :digest) (digest b))
      (str uri ": digest " (digest b) ", the manifest says " (field line :digest)))))

(defn content-bytes
  "The raw bytes of one `resources/read` contents item (keyword or
   string keys): `text` as UTF-8, or `blob` Base64-decoded; nil when it
   holds neither."
  ^bytes [item]
  (let [text (field item :text)
        blob (field item :blob)]
    (cond
      (string? text) (.getBytes ^String text StandardCharsets/UTF_8)
      (string? blob) (.decode (Base64/getDecoder) ^String blob)
      :else nil)))
