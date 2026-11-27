;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.ssh-sign
  "Signs bytes with an SSH key and verifies OpenSSH detached signatures
   (SSHSIG), the format `ssh-keygen -Y` reads and writes.

   Signing always runs `ssh-keygen -Y sign`: a private key, a security key
   or an agent is ssh-keygen's business, so no private key is ever read
   here. The message goes to ssh-keygen on stdin and the signature comes
   back on stdout, so signing writes no file.

   Verifying is done in process by `briko.sshsig` for the key types it
   implements (`ssh-ed25519`, and `sk-ssh-ed25519@openssh.com` with the
   user-presence flag set), so a host without OpenSSH can still judge
   them. Only where briko.sshsig answers unsupported (RSA and ECDSA keys,
   an unknown hash, an allowed-signers line with `valid-before` or
   `valid-after`) does verification fall back to `ssh-keygen -Y verify`,
   when ssh-keygen is there. A verdict is true, false, or nil when neither
   reader could judge: ssh-keygen is missing, cannot be started or overran
   its deadline. nil is falsy, so a caller treating the verdict as a
   boolean fails closed, and a caller that tells \"unchecked\" from
   \"forged\" can.

   The signing namespace (`-n`) is an explicit argument of every function,
   so a signature made for one purpose never verifies for another by a
   shared default.

   Every ssh-keygen run goes through `briko.process`, within :timeout-ms
   (60 s by default, long enough for a security key waiting on a touch).
   The files ssh-keygen must read (a signature to verify, allowed signers
   given as text) are written to a fresh directory only its owner can
   enter, created with those permissions, and the directory is removed
   when the call returns, also when it throws.

   Failures to sign or read a public key throw an ex-info whose :type is
   in `refusal-types`. Invalid arguments and unknown options are refused
   the same way, so a misspelt option never falls back unnoticed."
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [briko.sshsig :as sshsig]
            [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file Files LinkOption OpenOption Path StandardOpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.security MessageDigest)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a call uses where the caller gives none.

   - :timeout-ms, the deadline of each ssh-keygen run in milliseconds:
     60000.
   - :ssh-keygen, the program to run: \"ssh-keygen\", looked up on PATH."
  {:timeout-ms 60000
   :ssh-keygen "ssh-keygen"})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-arguments ::invalid-options ::command-not-run ::command-timed-out
    ::command-failed ::invalid-output ::io-failed})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

(def ^:private max-output-bytes
  "The most stdout a run may return: a signature or public key line is
   well under a kilobyte, even for an RSA key."
  65536)

;; Arguments

(defn- non-blank? [x] (and (string? x) (not (str/blank? x))))

(def ^:private option-checks
  {:timeout-ms [(fn [x] (and (int? x) (< 0 (long x) Integer/MAX_VALUE)))
                "a positive int below Integer/MAX_VALUE milliseconds"]
   :ssh-keygen [non-blank? "a non-blank string"]})

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

(defn- path-string
  "A file argument as a string: a non-blank string, a File or a Path."
  [argument x]
  (let [s (cond (string? x) x
                (or (instance? File x) (instance? Path x)) (str x))]
    (when-not (non-blank? s)
      (refuse! ::invalid-arguments (str (name argument) " must be a non-blank string, a File or a Path")
               {:argument argument}))
    s))

(defn- check-bytes! [argument x]
  (when-not (bytes? x)
    (refuse! ::invalid-arguments (str (name argument) " must be a byte array") {:argument argument})))

(defn- check-namespace! [namespace]
  (when-not (non-blank? namespace)
    (refuse! ::invalid-arguments "the namespace must be a non-blank string" {:argument :namespace})))

(defn- check-signature! [signature]
  (when-not (or (bytes? signature) (string? signature))
    (refuse! ::invalid-arguments "the signature must be a byte array or a string" {:argument :signature})))

(defn- signature-bytes ^bytes [signature]
  (if (bytes? signature) signature (.getBytes ^String signature "UTF-8")))

;; Temporary files

(def ^:private owner-only-directory
  (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rwx------")))

(def ^:private owner-only-file
  (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------")))

(defn- with-private-directory
  "Calls f with a fresh directory only its owner can enter, created with
   those permissions in the same call, and removes it with everything in
   it when f returns or throws."
  [f]
  (let [^Path dir (try
                    (Files/createTempDirectory "briko-ssh-sign" (into-array FileAttribute [owner-only-directory]))
                    (catch Exception e
                      (refuse! ::io-failed "could not create a private temporary directory" {} e)))]
    (try
      (f dir)
      (finally
        (fs/delete-tree dir)))))

(def ^:private write-options
  (into-array OpenOption [StandardOpenOption/WRITE LinkOption/NOFOLLOW_LINKS]))

(defn- write-private!
  "Writes bytes to a new file n in dir, readable by its owner only. The
   file must not exist yet, so nothing planted there is written through."
  ^Path [^Path dir ^String n ^bytes content]
  (let [p (.resolve dir n)]
    (try
      (Files/createFile p (into-array FileAttribute [owner-only-file]))
      (Files/write p content ^"[Ljava.nio.file.OpenOption;" write-options)
      (catch Exception e
        (refuse! ::io-failed (str "could not write the temporary file " n) {} e)))))

;; ssh-keygen

(defn- ssh-keygen
  "Runs ssh-keygen with args and the given stdin within the deadline."
  [{:keys [timeout-ms] :as opts} args in]
  (process/run (into [(:ssh-keygen opts)] args)
               {:timeout-ms timeout-ms
                :in in
                :max-out-bytes max-output-bytes}))

(defn- judged?
  "Whether a run of ssh-keygen got as far as answering."
  [{:keys [launched? timed-out?]}]
  (and launched? (not timed-out?)))

(defn- checked-output
  "The stdout of a run that succeeded; refuses one that did not."
  ^String [opts what {:keys [exit out err launched? timed-out? out-truncated?] :as _result}]
  (cond
    (not launched?)
    (refuse! ::command-not-run (str "could not run " (:ssh-keygen opts) " to " what)
             {:program (:ssh-keygen opts) :err err})
    timed-out?
    (refuse! ::command-timed-out (str (:ssh-keygen opts) " did not " what " within " (:timeout-ms opts) " ms")
             {:program (:ssh-keygen opts) :timeout-ms (:timeout-ms opts)})
    (not (zero? (long exit)))
    (refuse! ::command-failed (str (:ssh-keygen opts) " could not " what ", exit " exit)
             {:program (:ssh-keygen opts) :exit exit :err (str/trim (str err))})
    out-truncated?
    (refuse! ::invalid-output (str (:ssh-keygen opts) " wrote more than " max-output-bytes " bytes")
             {:program (:ssh-keygen opts) :limit max-output-bytes})
    :else out))

(def ^:private availability (atom {}))

(defn ssh-keygen-available?
  "Whether the ssh-keygen of `opts` can be started here. Asked once per
   program and remembered, since the answer does not change during a run
   and a service checking many signatures should not fork per question.
   The exit code does not matter: ssh-keygen refusing bogus arguments
   still proves it ran."
  ([] (ssh-keygen-available? nil))
  ([opts]
   (let [{program :ssh-keygen :as opts} (options opts)]
     (if (contains? @availability program)
       (get @availability program)
       (let [answer (boolean (:launched? (ssh-keygen opts ["-Y" "check-novalidate" "-n" "x" "-s" "/nonexistent"] "")))]
         (swap! availability assoc program answer)
         answer)))))

;; Keys

(defn pubkey
  "The OpenSSH public key line (`ssh-ed25519 AAAA… comment`) of the
   private key file `key-file`, read by `ssh-keygen -y`."
  ([key-file] (pubkey key-file nil))
  ([key-file opts]
   (let [key-file (path-string :key-file key-file)
         opts (options opts)
         line (str/trim (checked-output opts "read the public key"
                                        (ssh-keygen opts ["-y" "-f" key-file] nil)))]
     (when-not (and (not (str/includes? line "\n")) (sshsig/key-blob line))
       (refuse! ::invalid-output (str (:ssh-keygen opts) " did not print one public key line")
                {:program (:ssh-keygen opts)}))
     line)))

(defn fingerprint
  "The SHA-256 fingerprint of an OpenSSH public key line, as
   `ssh-keygen -l` prints it: `SHA256:` and the unpadded base64 of the
   SHA-256 over the key blob. Computed in process; refuses a line that
   carries no key."
  [pubkey-line]
  (let [blob (when (string? pubkey-line) (sshsig/key-blob pubkey-line))]
    (when-not blob
      (refuse! ::invalid-arguments "not an OpenSSH public key line" {:argument :pubkey-line}))
    (str "SHA256:" (.encodeToString (.withoutPadding (Base64/getEncoder))
                                    (.digest (MessageDigest/getInstance "SHA-256") ^bytes blob)))))

;; Signing

(defn sign-bytes
  "The armored SSHSIG signature (as bytes) over the byte array `message`
   for `namespace`, made by `ssh-keygen -Y sign -f key-file`. key-file is
   a private key, or a public key whose private half an agent or a
   security key holds."
  (^bytes [key-file message namespace] (sign-bytes key-file message namespace nil))
  (^bytes [key-file message namespace opts]
   (let [key-file (path-string :key-file key-file)]
     (check-bytes! :message message)
     (check-namespace! namespace)
     (let [opts (options opts)
           out (checked-output opts "sign"
                               (ssh-keygen opts ["-Y" "sign" "-f" key-file "-n" namespace] message))]
       (when-not (str/starts-with? out "-----BEGIN SSH SIGNATURE-----")
         (refuse! ::invalid-output (str (:ssh-keygen opts) " did not print a signature")
                  {:program (:ssh-keygen opts)}))
       (.getBytes out "UTF-8")))))

;; Verifying

(defn- ssh-keygen-verify
  "ssh-keygen's verdict, or nil when it did not get to answer."
  [opts allowed-signers ^bytes message ^bytes signature principal namespace]
  (with-private-directory
    (fn [dir]
      (let [sig (write-private! dir "signature" signature)
            result (ssh-keygen opts ["-Y" "verify" "-f" allowed-signers "-I" principal
                                     "-n" namespace "-s" (str sig)]
                               message)]
        (when (judged? result)
          (zero? (long (:exit result))))))))

(defn- ssh-keygen-principals
  [opts allowed-signers ^bytes message ^bytes signature namespace]
  (with-private-directory
    (fn [dir]
      (let [sig (write-private! dir "signature" signature)
            result (ssh-keygen opts ["-Y" "find-principals" "-f" allowed-signers
                                     "-n" namespace "-s" (str sig)]
                               message)]
        (when (judged? result)
          (if (zero? (long (:exit result)))
            (vec (remove str/blank? (str/split-lines (:out result))))
            []))))))

(defn- registry-text
  "The allowed-signers file as text, or nil when there is no such file."
  [^String path]
  (let [f (File. path)]
    (when (.isFile f) (slurp f :encoding "UTF-8"))))

(defn- judged
  "The in-process verdict when briko.sshsig could judge, else nil."
  [verdict]
  (when-not (or (nil? verdict) (sshsig/unsupported? verdict)) verdict))

(defn verify-text
  "Whether `signature` (armored, bytes or string) endorses the byte array
   `message` for `namespace` under a key that `allowed-signers-text` (the
   allowed-signers file as text) grants to `principal`: true, false, or
   nil when neither reader could judge. briko.sshsig judges in process;
   only a signature it cannot judge is handed to ssh-keygen, which reads
   the text from a private temporary copy."
  ([allowed-signers-text message signature principal namespace]
   (verify-text allowed-signers-text message signature principal namespace nil))
  ([allowed-signers-text message signature principal namespace opts]
   (when-not (string? allowed-signers-text)
     (refuse! ::invalid-arguments "the allowed signers must be a string" {:argument :allowed-signers-text}))
   (check-bytes! :message message)
   (check-signature! signature)
   (check-namespace! namespace)
   (let [opts (options opts)
         in-process (sshsig/verify allowed-signers-text message signature principal namespace)]
     (cond
       (boolean? in-process) in-process
       (not (ssh-keygen-available? opts)) nil
       :else (with-private-directory
               (fn [dir]
                 (let [signers (write-private! dir "allowed-signers" (.getBytes ^String allowed-signers-text "UTF-8"))]
                   (ssh-keygen-verify opts (str signers) message (signature-bytes signature)
                                      principal namespace))))))))

(defn verify-bytes
  "Whether `signature` (armored, bytes or string) endorses the byte array
   `message` for `namespace` under a key the allowed-signers file at
   `allowed-signers` (a string, File or Path) grants to `principal`: true,
   false, or nil when neither reader could judge. briko.sshsig judges in
   process; a signature it cannot judge, or a registry that is not a
   regular file, is handed to ssh-keygen."
  ([allowed-signers message signature principal namespace]
   (verify-bytes allowed-signers message signature principal namespace nil))
  ([allowed-signers message signature principal namespace opts]
   (let [allowed-signers (path-string :allowed-signers allowed-signers)]
     (check-bytes! :message message)
     (check-signature! signature)
     (check-namespace! namespace)
     (let [opts (options opts)
           text (registry-text allowed-signers)
           in-process (when text (sshsig/verify text message signature principal namespace))]
       (cond
         (boolean? in-process) in-process
         ;; a missing principal never stands for any principal
         (not (non-blank? principal)) false
         (not (ssh-keygen-available? opts)) nil
         :else (ssh-keygen-verify opts allowed-signers message (signature-bytes signature)
                                  principal namespace))))))

(defn find-principals
  "The principals the allowed-signers file at `allowed-signers` names on
   the lines granting the key that produced `signature` over `message` for
   `namespace`: a vector, empty when none, or nil when neither reader
   could judge. In process the signature is verified first. The
   ssh-keygen fallback (`-Y find-principals`) matches the signature's key
   against the registry without checking the signature over the message;
   use `verify-bytes` for a verdict on the message."
  ([allowed-signers message signature namespace]
   (find-principals allowed-signers message signature namespace nil))
  ([allowed-signers message signature namespace opts]
   (let [allowed-signers (path-string :allowed-signers allowed-signers)]
     (check-bytes! :message message)
     (check-signature! signature)
     (check-namespace! namespace)
     (let [opts (options opts)
           text (registry-text allowed-signers)
           in-process (judged (when text (sshsig/find-principals text message signature namespace)))]
       (cond
         (some? in-process) in-process
         (not (ssh-keygen-available? opts)) nil
         :else (ssh-keygen-principals opts allowed-signers message (signature-bytes signature) namespace))))))
