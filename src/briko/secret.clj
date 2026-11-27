;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.secret
  "Resolves secrets named in configuration at the moment of use, so a
   configuration file can be committed with no secret in it.

   A secret is a plain string, or a map with exactly one of these keys
   naming where to fetch it:

     {:env \"NAME\"}                   a process environment variable
     {:file \"/run/secrets/x\"}        the first line of a file, trimmed;
                                       Compose and mounted Kubernetes
                                       secrets land at a fixed path
     {:credential \"NAME\"}            the first line of the systemd
                                       credential NAME, the file of that
                                       name in $CREDENTIALS_DIRECTORY
     {:command [\"pass\" \"show\" \"x\"]}  the first line a password manager
                                       prints; a vector runs without a
                                       shell, a string runs as `sh -c`

   Resolution fails loud: an unset variable, a missing file, a command
   that fails or overruns its deadline, and a value that is empty or blank
   all throw, because a blank secret only turns into a puzzling 401 later.
   Every refusal is an ex-info whose data carries a stable :type, one of
   `refusal-types`, and the name of the secret under :secret. No refusal
   carries a secret value or the output of a command: a failing command's
   stdout is discarded, and its stderr is never captured but passed to
   this process's stderr, where the command's own diagnostics reach the
   operator exactly as when run by hand.

   An environment variable is used as it is set. A file, a credential and
   a command's output yield their first line with surrounding whitespace
   trimmed, the password-manager convention of the password on line 1. A
   blank first line is refused rather than skipped, because the lines
   below it hold fields such as `login: x`, never the password.

   The single-key rule keeps a spec apart from ordinary configuration that
   happens to carry one of these keys next to others: `resolve-secrets`
   leaves such a map alone and only walks into its values."
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [clojure.string :as str])
  (:import (java.io IOException InputStream)
           (java.nio ByteBuffer)
           (java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets)
           (java.nio.file FileAlreadyExistsException Files LinkOption OpenOption StandardOpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a resolution uses where the caller gives none.

   - :timeout-ms, the deadline of a :command in milliseconds: 60000, long
     enough for a password manager waiting on a hardware key or an unlock.
   - :max-bytes, the longest file, credential or command output read:
     64 KiB, which holds any password, token or SSH private key. Longer
     input is refused rather than cut.
   - :getenv, a function from a variable name to its value or nil:
     System/getenv. It reads :env specs and $CREDENTIALS_DIRECTORY; a
     command still inherits the real environment."
  {:timeout-ms 60000
   :max-bytes 65536
   :getenv (fn [n] (System/getenv ^String n))})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-spec ::invalid-name ::unset ::empty ::missing
    ::not-a-file ::too-large ::not-text ::unreadable ::no-credentials-directory
    ::command-not-run ::command-failed ::command-timed-out ::refused-source
    ::invalid-directory ::insecure-directory ::exists})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

;; Options

(def ^:private option-checks
  {:timeout-ms [(fn [x] (and (int? x) (< 0 (long x) Integer/MAX_VALUE))) "a positive int"]
   :max-bytes [(fn [x] (and (int? x) (< 0 (long x) Integer/MAX_VALUE))) "a positive int below Integer/MAX_VALUE"]
   :getenv [ifn? "a function"]})

(defn- options
  [opts]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove (set (keys defaults)) (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [[k [valid? wanted]] option-checks]
      (when-not (valid? (get opts k))
        (refuse! ::invalid-options (str k " must be " wanted) {:option k})))
    opts))

;; Text helpers

(defn first-line
  "The first line of s with surrounding whitespace trimmed, or nil when s
   is nil or its first line is blank."
  [s]
  (when (some? s)
    (let [line (str/trim (or (first (str/split-lines (str s))) ""))]
      (when-not (str/blank? line) line))))

(defn field-in
  "The trimmed value of the first `name: value` line of s whose name is
   field-name, or nil when there is none or its value is blank. pass,
   passage and pasejo keep named fields such as a login below the
   password this way."
  [s field-name]
  (some (fn [line]
          (let [[k v] (str/split line #":" 2)]
            (when (and v (= (str/trim k) field-name))
              (let [v (str/trim v)]
                (when-not (str/blank? v) v)))))
        (str/split-lines (str s))))

;; Specs

(def ^:private spec-keys #{:env :file :credential :command})

(defn spec?
  "Whether x is a secret spec: a map with exactly one key, which names a
   source."
  [x]
  (and (map? x) (= 1 (count x)) (contains? spec-keys (key (first x)))))

(defn- shape
  "x described without its value, which may be a secret put in the wrong
   place."
  [x]
  (cond
    (map? x) (str "a map with the keys " (pr-str (vec (keys x))))
    (nil? x) "nil"
    :else (str "a " (.getName (class x)))))

(defn- no-nul?
  [^String s]
  (neg? (.indexOf s (int 0))))

(defn- bare-name?
  "Whether n names a file directly inside a directory: no separator, no
   `..`, no NUL, and not `.`."
  [n]
  (and (string? n)
       (not (str/blank? n))
       (no-nul? n)
       (not= "." n)
       (not (re-find #"[/\\]|\.\." n))))

(defn- argv-of
  [what c]
  (cond
    (and (string? c) (not (str/blank? c))) ["sh" "-c" c]
    (and (sequential? c) (seq c) (every? string? c) (not (str/blank? (first c)))) (vec c)
    :else (refuse! ::invalid-spec
                   (str what " :command must be a shell string or a non-empty vector of strings, not " (shape c))
                   {:secret what})))

(defn- check-spec!
  [what spec]
  (when-not (spec? spec)
    (refuse! ::invalid-spec
             (str what " is not a secret: want a string or a map with one of the keys "
                  ":env, :file, :credential or :command, not " (shape spec))
             {:secret what}))
  (let [[k v] (first spec)]
    (case k
      :env (when-not (and (string? v) (not (str/blank? v)) (no-nul? v) (not (str/includes? v "=")))
             (refuse! ::invalid-spec (str what " :env must name a variable, not " (shape v)) {:secret what}))
      :file (when-not (and (string? v) (not (str/blank? v)) (no-nul? v))
              (refuse! ::invalid-spec (str what " :file must be a path, not " (shape v)) {:secret what}))
      :credential (when-not (bare-name? v)
                    (refuse! ::invalid-name
                             (str what " credential name " (if (string? v) (pr-str v) (shape v))
                                  " must be a bare name (no slashes, no .., no NUL)")
                             {:secret what :credential (when (string? v) v)}))
      :command (argv-of what v))
    spec))

;; Sources

(defn- decode!
  ^String [what ^bytes bs data]
  (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (try
      (str (.decode decoder (ByteBuffer/wrap bs)))
      (catch CharacterCodingException e
        (refuse! ::not-text (str what " is not UTF-8 text") data e)))))

(defn- read-file!
  "The whole text of the regular file at path, at most :max-bytes long.
   A FIFO or a device is refused, so the read cannot block."
  ^String [what path data {:keys [max-bytes]}]
  (cond
    (not (fs/exists? path))
    (refuse! ::missing (str what " file " path " does not exist") data)

    (not (fs/regular-file? path))
    (refuse! ::not-a-file (str what " file " path " is not a regular file") data))
  (let [bs (try
             (with-open [^InputStream in (Files/newInputStream (fs/path path) (make-array OpenOption 0))]
               (.readNBytes in (int (inc (long max-bytes)))))
             (catch IOException e
               (refuse! ::unreadable (str what " file " path " cannot be read: " (ex-message e)) data e)))]
    (when (> (alength ^bytes bs) (long max-bytes))
      (refuse! ::too-large (str what " file " path " is larger than " max-bytes " bytes")
               (assoc data :limit max-bytes)))
    (decode! what bs data)))

(defn- credential-path
  [what n {:keys [getenv]}]
  (let [dir (getenv "CREDENTIALS_DIRECTORY")]
    (when (or (str/blank? dir) (not (fs/absolute? dir)))
      (refuse! ::no-credentials-directory
               (str what " wants systemd credential " (pr-str n)
                    " but $CREDENTIALS_DIRECTORY is not set to an absolute path;"
                    " the process does not run with systemd credentials"
                    " (LoadCredential=, LoadCredentialEncrypted=, SetCredential=)")
               {:secret what :credential n}))
    (str (fs/path dir n))))

(defn- read-credential!
  ^String [what n opts]
  (let [path (credential-path what n opts)
        data {:secret what :credential n}]
    (when-not (fs/exists? path)
      (refuse! ::missing
               (str what " systemd credential " (pr-str n) " is not present in $CREDENTIALS_DIRECTORY;"
                    " declare it with LoadCredential=" n ":…")
               data))
    (read-file! what path data opts)))

(defn- run-command!
  "The whole stdout of the command spec c, after it exited 0 in time."
  ^String [what c {:keys [timeout-ms max-bytes]}]
  (let [argv (argv-of what c)
        program (first argv)
        data {:secret what :program program}
        {:keys [exit out err timed-out? launched? out-truncated?]}
        (process/run argv {:timeout-ms timeout-ms
                           :max-out-bytes (inc (long max-bytes))
                           :inherit-err? true})]
    (cond
      (not launched?)
      (refuse! ::command-not-run (str what " command could not be run: " err) data)

      timed-out?
      (refuse! ::command-timed-out
               (str what " command " program " ran longer than " timeout-ms " ms and was stopped")
               (assoc data :timeout-ms timeout-ms))

      (not (zero? (long exit)))
      (refuse! ::command-failed (str what " command " program " exited with " exit) (assoc data :exit exit))

      (or out-truncated? (> (alength (.getBytes ^String out StandardCharsets/UTF_8)) (long max-bytes)))
      (refuse! ::too-large (str what " command " program " printed more than " max-bytes " bytes")
               (assoc data :limit max-bytes))

      :else out)))

(defn- non-blank!
  "The first line of value, refusing a blank one."
  [what value data]
  (or (first-line value)
      (refuse! ::empty (str what " is empty") data)))

(defn- whole-non-blank!
  "value, refusing one that is blank throughout."
  [what value data]
  (if (str/blank? value)
    (refuse! ::empty (str what " is empty") data)
    value))

(defn- resolve-checked
  [what spec opts]
  (let [[k v] (first spec)]
    (case k
      :env (let [value ((:getenv opts) v)
                 data {:secret what :env v}]
             (cond
               (nil? value) (refuse! ::unset (str what " wants environment variable " v ", which is not set") data)
               (str/blank? value) (refuse! ::empty (str what " wants environment variable " v ", which is empty") data)
               :else value))
      :file (non-blank! what (read-file! what v {:secret what :file v} opts) {:secret what :file v})
      :credential (non-blank! what (read-credential! what v opts) {:secret what :credential v})
      :command (non-blank! what (run-command! what v opts) {:secret what :program (first (argv-of what v))}))))

(defn resolve1
  "The value of one secret: a string as it is, or the value the spec
   names, read with the options in `defaults`. what names the secret in
   refusals. nil passes through, for an optional setting left out; any
   other value that is neither a string nor a spec is refused."
  ([what spec] (resolve1 what spec nil))
  ([what spec opts]
   (let [opts (options opts)]
     (if (or (nil? spec) (string? spec))
       spec
       (resolve-checked what (check-spec! what spec) opts)))))

(defn- key-name
  [k]
  (if (keyword? k) (name k) (str k)))

(defn- walk
  [what x opts]
  (cond
    (spec? x) (resolve-checked what (check-spec! what x) opts)
    ;; Associating onto x itself keeps records, sorted maps and metadata.
    (map? x) (reduce-kv (fn [m k v] (assoc m k (walk (key-name k) v opts))) x x)
    (vector? x) (mapv #(walk what % opts) x)
    (seq? x) (doall (map #(walk what % opts) x))
    (set? x) (into (empty x) (map #(walk what % opts)) x)
    :else x))

(defn resolve-secrets
  "x with every secret spec anywhere inside it resolved and everything
   else untouched. A spec under a map key is named after that key in
   refusals, so an error names the field (\"Authorization\", \"url\"). A
   map that is not a spec, such as one carrying :command next to other
   keys, is kept and walked into."
  ([x] (resolve-secrets nil x nil))
  ([what x] (resolve-secrets what x nil))
  ([what x opts]
   (walk (or what "secret") x (options opts))))

;; Files

(defn- check-private-dir!
  [what dir]
  (when-not (and (some? dir) (fs/directory? dir))
    (refuse! ::invalid-directory (str what " needs an existing directory to write into") {:secret what}))
  (let [perms (try
                (fs/posix->str (fs/posix-file-permissions dir))
                (catch UnsupportedOperationException e
                  (refuse! ::insecure-directory
                           (str what " cannot check that the directory is private: no POSIX permissions")
                           {:secret what :dir (str dir)} e)))]
    (when-not (str/ends-with? perms "------")
      (refuse! ::insecure-directory
               (str what " directory " dir " is " perms "; it must be accessible to its owner only")
               {:secret what :dir (str dir) :permissions perms}))))

(defn- write-owner-only!
  "Creates the file n in dir holding content, readable by its owner only
   from the moment it exists. An existing file or link of that name is
   refused, never followed or replaced."
  [what dir n ^String content]
  (let [p (fs/path dir n)]
    (try
      (Files/createFile p (into-array FileAttribute
                                      [(PosixFilePermissions/asFileAttribute
                                        (PosixFilePermissions/fromString "rw-------"))]))
      (catch FileAlreadyExistsException e
        (refuse! ::exists (str what " file " p " already exists") {:secret what :file (str p)} e))
      (catch IOException e
        (refuse! ::unreadable (str what " file " p " cannot be created: " (ex-message e))
                 {:secret what :file (str p)} e)))
    (try
      (with-open [out (Files/newOutputStream p (into-array OpenOption [StandardOpenOption/WRITE
                                                                       LinkOption/NOFOLLOW_LINKS]))]
        (.write out (.getBytes content StandardCharsets/UTF_8)))
      (catch IOException e
        (Files/deleteIfExists p)
        (refuse! ::unreadable (str what " file " p " cannot be written: " (ex-message e))
                 {:secret what :file (str p)} e)))
    (str p)))

(defn materialize-file
  "The path of a file holding a secret, for a consumer that needs a file,
   such as an SSH private key handed to `ssh-keygen -f`. what names the
   secret in refusals, which never carry it.

     \"/path\" or {:file \"/path\"}   that file, where it lies
     {:credential \"NAME\"}           the systemd credential's file
     {:command [...]} or {:command \"...\"}
                                     the command's whole stdout, written
                                     to a new file n inside dir, which
                                     must be accessible to its owner only

   Each file or output is read first and refused when missing, empty,
   blank throughout, not UTF-8 or longer than :max-bytes; unlike
   `resolve1`, the whole content counts, not its first line. {:env …} is refused, because every
   child of the process inherits a variable."
  ([what spec dir n] (materialize-file what spec dir n nil))
  ([what spec dir n opts]
   (let [opts (options opts)
         spec (if (string? spec) {:file spec} spec)]
     (check-spec! what spec)
     (let [[k v] (first spec)]
       (case k
         :file (do (whole-non-blank! what (read-file! what v {:secret what :file v} opts) {:secret what :file v})
                   v)
         :credential (do (whole-non-blank! what (read-credential! what v opts) {:secret what :credential v})
                         (credential-path what v opts))
         :command (let [_ (when-not (bare-name? n)
                            (refuse! ::invalid-name (str what " file name must be a bare name (no slashes, no .., no NUL)")
                                     {:secret what}))
                        _ (check-private-dir! what dir)
                        out (run-command! what v opts)
                        _ (whole-non-blank! what out {:secret what :program (first (argv-of what v))})]
                    (write-owner-only! what dir n (if (str/ends-with? out "\n") out (str out "\n"))))
         :env (refuse! ::refused-source
                       (str what " must be a file path, {:file …}, {:credential …} or {:command …};"
                            " {:env …} is refused for a file, because every child process inherits it")
                       {:secret what}))))))
