;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.fs
  "Writes files so that no reader sees them half-written and no other
   user can read what is private.

   `atomic-write!` replaces a file in one step: the content goes into a
   new temporary file in the same directory, created under a random name
   that no other writer can share, and is renamed over the target only
   once it is complete. A reader sees the old content or the new, never a
   mix, and two writers racing on one path each install a whole file.
   When the file system cannot rename atomically, the write is refused;
   it never falls back to a copy that a reader could see half done.

   `write-private!` and `create-private-directory!` create a file
   rw------- or a directory rwx------ with the permissions given in the
   same system call that creates it, so no other user can open it in
   between, whatever the umask. Neither replaces anything that exists, a
   symbolic link included, so a planted file or link is refused rather
   than written through.

   A file system without POSIX permissions, or one that ignores the mode
   it is given (such as vfat on Linux), cannot keep a file private. Every
   private creation reads the permissions back before writing anything
   and refuses with ::not-posix, removing what it created, when they are
   wider than asked for.

   Content is a string, written as UTF-8, or a byte array. Every refusal
   is an ex-info whose data carries a stable :type, one of
   `refusal-types`, and the :file it concerns."
  (:require [babashka.fs :as fs]
            [clojure.string :as str])
  (:import (java.io IOException)
           (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.nio.charset StandardCharsets)
           (java.nio.file CopyOption FileAlreadyExistsException Files LinkOption OpenOption Path
                          StandardCopyOption StandardOpenOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util HashSet Set UUID)))

(set! *warn-on-reflection* true)

(def defaults
  "The options a write uses where the caller gives none.

   - :fsync?, whether the data reaches the disk before the call returns:
     true. `atomic-write!` forces the temporary file before the rename and
     the directory after it, so the new content survives a crash once the
     call has returned; `write-private!` forces the file. A caller writing
     a cache or other content it can rebuild passes false.
   - :private?, for `atomic-write!` only, whether the file is created
     rw-------: false, which creates it with the mode a new file gets
     under the umask."
  {:fsync? true
   :private? false})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-input ::no-directory ::exists ::not-posix ::not-atomic ::failed})

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
  (let [opts (merge defaults opts)]
    (doseq [k allowed]
      (when-not (boolean? (get opts k))
        (refuse! ::invalid-options (str k " must be true or false") {:option k})))
    opts))

(defn- path-of
  ^Path [p]
  (when-not (or (and (string? p) (not (str/blank? p))) (instance? Path p) (instance? java.io.File p))
    (refuse! ::invalid-input "the path must be a non-blank string, a Path or a File" {}))
  (.toAbsolutePath (fs/path p)))

(defn- bytes-of
  ^bytes [content]
  (cond
    (string? content) (.getBytes ^String content StandardCharsets/UTF_8)
    (bytes? content) content
    :else (refuse! ::invalid-input "the content must be a string or a byte array" {})))

(defn- io-message
  [^Throwable e]
  (or (ex-message e) (.getName (class e))))

;; Permissions

(def ^:private private-file "rw-------")
(def ^:private private-dir "rwx------")

(defn- attrs
  ^"[Ljava.nio.file.attribute.FileAttribute;" [spec]
  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString spec))]))

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- permissions-of
  "The POSIX permissions of p, not following a link, or nil when the
   file system has none."
  [^Path p]
  (try
    (Files/getPosixFilePermissions p no-follow)
    (catch UnsupportedOperationException _ nil)))

(defn- check-private!
  "Refuses, after deleting p, when p carries a permission spec does not
   grant. Runs before anything is written to p."
  [^Path p spec]
  (let [granted (PosixFilePermissions/fromString spec)
        actual (permissions-of p)]
    (when-not (and actual (every? #(contains? granted %) actual))
      (Files/deleteIfExists p)
      (refuse! ::not-posix
               (str p " cannot be kept private: the file system "
                    (if actual
                      (str "gave it " (PosixFilePermissions/toString actual) " instead of " spec)
                      "has no POSIX permissions"))
               {:file (str p)}))))

(defn- unsupported-attrs!
  [^Path p ^Throwable e]
  (refuse! ::not-posix (str p " cannot be kept private: the file system has no POSIX permissions")
           {:file (str p)} e))

;; Writing through a channel

(defn- open-new
  "A channel on a new file at p, created with CREATE_NEW, so an existing
   file or symbolic link at p is refused with
   FileAlreadyExistsException."
  ^FileChannel [^Path p private?]
  (let [options (doto (HashSet.)
                  (.add StandardOpenOption/CREATE_NEW)
                  (.add StandardOpenOption/WRITE)
                  (.add LinkOption/NOFOLLOW_LINKS))]
    (if private?
      (try
        (FileChannel/open p ^Set options (attrs private-file))
        (catch UnsupportedOperationException e (unsupported-attrs! p e)))
      (FileChannel/open p ^Set options (make-array FileAttribute 0)))))

(defn- write-all!
  [^FileChannel channel ^bytes bs]
  (let [buffer (ByteBuffer/wrap bs)]
    (while (.hasRemaining buffer)
      (.write channel buffer))))

(defn- force-directory!
  [^Path dir]
  (with-open [channel (FileChannel/open dir (into-array OpenOption [StandardOpenOption/READ]))]
    (.force channel true)))

(defn write-private!
  "Writes content to a new file at path, rw------- from the moment it
   exists, and returns the path as a string. Refuses with ::exists when
   anything is at path, a symbolic link included, and with ::not-posix
   on a file system that cannot keep the file private. Options :fsync?
   (see `defaults`). A write that fails removes the file it created."
  ([path content] (write-private! path content nil))
  ([path content opts]
   (let [{:keys [fsync?]} (options opts #{:fsync?})
         p (path-of path)
         bs (bytes-of content)
         channel (try
                   (open-new p true)
                   (catch FileAlreadyExistsException e
                     (refuse! ::exists (str p " already exists") {:file (str p)} e))
                   (catch IOException e
                     (refuse! ::failed (str p " cannot be created: " (io-message e)) {:file (str p)} e)))]
     (try
       (with-open [^FileChannel channel channel]
         (check-private! p private-file)
         (write-all! channel bs)
         (when fsync? (.force channel true)))
       (catch IOException e
         (Files/deleteIfExists p)
         (refuse! ::failed (str p " cannot be written: " (io-message e)) {:file (str p)} e))
       ;; Whatever was thrown, the file this call created is removed first.
       #_{:splint/disable [lint/catch-throwable]}
       (catch Throwable e
         (Files/deleteIfExists p)
         (throw e)))
     (str p))))

(defn create-private-directory!
  "Creates the directory dir rwx------, and every missing parent the same
   way, and returns its path as a string. Refuses with ::exists when
   anything is at dir: an existing directory may belong to somebody else
   or grant more. Refuses with ::not-posix, removing dir, on a file system
   that cannot keep it private."
  [dir]
  (let [p (path-of dir)
        parent (.getParent p)]
    (try
      (when (and parent (not (Files/isDirectory parent (make-array LinkOption 0))))
        (Files/createDirectories parent (attrs private-dir)))
      (Files/createDirectory p (attrs private-dir))
      (catch FileAlreadyExistsException e
        (refuse! ::exists (str p " already exists") {:file (str p)} e))
      (catch UnsupportedOperationException e
        (unsupported-attrs! p e))
      (catch IOException e
        (refuse! ::failed (str p " cannot be created: " (io-message e)) {:file (str p)} e)))
    (check-private! p private-dir)
    (str p)))

;; Atomic replacement

(defn- temp-name
  "A name for the temporary file of target: hidden, marked as briko's,
   and unique by the 122 random bits of a random UUID, so no two writers
   pick the same one. UUID/randomUUID draws from a SecureRandom the JDK
   creates on first use, which keeps a seeded generator out of a native
   image's heap."
  [^String target-name]
  (str "." target-name ".briko-" (str/replace (str (UUID/randomUUID)) "-" "") ".tmp"))

(defn- atomic-move-unsupported?
  "Whether e is java.nio.file.AtomicMoveNotSupportedException, named by
   string because babashka does not expose the class."
  [e]
  (= "java.nio.file.AtomicMoveNotSupportedException" (.getName (class e))))

(defn- move!
  [^Path from ^Path to]
  (Files/move from to (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))

(defn- write-temp!
  "Creates the temporary file beside target and writes content to it,
   returning its path. Retries a name that already exists, which only a
   planted file can cause."
  ^Path [^Path dir ^Path target content private? fsync?]
  (loop [attempt 0]
    (let [temp (.resolve dir ^String (temp-name (str (.getFileName target))))
          channel (try
                    (open-new temp private?)
                    (catch FileAlreadyExistsException e
                      (when (>= attempt 3)
                        (refuse! ::failed (str "no free temporary name beside " target) {:file (str target)} e))
                      nil))]
      (if-not channel
        (recur (inc attempt))
        (do
          (try
            (with-open [^FileChannel channel channel]
              (when private? (check-private! temp private-file))
              (if (fn? content)
                (content channel)
                (write-all! channel (bytes-of content)))
              (when fsync? (.force channel true)))
            ;; Whatever was thrown, the file this call created is removed first.
            #_{:splint/disable [lint/catch-throwable]}
            (catch Throwable e
              (Files/deleteIfExists temp)
              (throw e)))
          temp)))))

(defn atomic-write!
  "Replaces the file at path with content in one atomic rename and
   returns the path as a string. content is a string (UTF-8), a byte
   array, or a function of a java.nio.channels.FileChannel that writes
   the content itself, for content too large to hold in memory.

   The directory of path must exist. What is at path is replaced, a
   symbolic link included: the link is replaced by the file, its target
   is left alone. The new file gets the mode a new file gets under the
   umask, or rw------- with :private?; nothing of the old file's mode or
   owner carries over.

   Refuses with ::not-atomic when the file system cannot rename
   atomically, ::no-directory when the directory is missing, ::not-posix
   when :private? is asked of a file system that cannot keep the file
   private, and ::failed when writing or renaming fails. In every case,
   and when a content function throws, the temporary file is removed and
   the file at path is untouched. A content function's own exception is
   rethrown as it is. Options :fsync? and :private? (see `defaults`)."
  ([path content] (atomic-write! path content nil))
  ([path content opts]
   (let [{:keys [fsync? private?]} (options opts #{:fsync? :private?})
         target (path-of path)
         dir (.getParent target)]
     (when-not (or (fn? content) (string? content) (bytes? content))
       (refuse! ::invalid-input "the content must be a string, a byte array or a function of a FileChannel" {}))
     (when-not (and dir (Files/isDirectory dir (make-array LinkOption 0)))
       (refuse! ::no-directory (str "the directory of " target " does not exist") {:file (str target)}))
     (let [temp (try
                  (write-temp! dir target content private? fsync?)
                  (catch IOException e
                    (refuse! ::failed (str target " cannot be written: " (io-message e)) {:file (str target)} e)))]
       (try
         (move! temp target)
         ;; Whatever was thrown, the file this call created is removed first.
         #_{:splint/disable [lint/catch-throwable]}
         (catch Throwable e
           (Files/deleteIfExists temp)
           (cond
             (atomic-move-unsupported? e)
             (refuse! ::not-atomic (str target " cannot be replaced atomically on this file system")
                      {:file (str target)} e)

             (instance? IOException e)
             (refuse! ::failed (str target " cannot be replaced: " (io-message e)) {:file (str target)} e)

             :else (throw e))))
       (when fsync?
         (try
           (force-directory! dir)
           (catch IOException e
             (refuse! ::failed (str "the directory of " target " cannot be forced to disk, so the new file"
                                    " is in place but may not survive a crash: " (io-message e))
                      {:file (str target)} e))))
       (str target)))))

(defn permissions
  "The POSIX permissions of path as a string such as \"rw-------\", not
   following a symbolic link, or nil on a file system without them."
  [path]
  (some-> (permissions-of (path-of path)) PosixFilePermissions/toString))
