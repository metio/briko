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
   it never falls back to a copy that a reader could see half done. With
   :mode the staged file carries its final permissions before the rename,
   so the path never shows the file with the umask's mode. With
   `:symlink :follow` a symbolic link at the path is followed and the file
   it ends at is replaced, so the link stays.

   `replace-symlink!` points a symbolic link somewhere else in one rename,
   so a reader of the path finds the old link or the new one and never
   nothing.

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
           (java.nio.file.attribute FileAttribute PosixFilePermission PosixFilePermissions)
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
     under the umask.
   - :mode, for `atomic-write!` only, the permissions of the new file: nil,
     the umask's mode. A string such as \"rwxr-xr-x\" or a set of
     java.nio.file.attribute.PosixFilePermission sets exactly those
     permissions, whatever the umask; :keep takes the permissions of the
     regular file the write replaces.
   - :symlink, for `atomic-write!` only, what a symbolic link at the path
     means: :replace, the link is replaced by the file. :follow writes the
     file the link chain ends at instead.
   - :root, for `atomic-write!` with `:symlink :follow` only, a directory
     the followed file must lie in: nil, anywhere.
   - :replace-file?, for `replace-symlink!` only, whether a regular file
     at the path may be replaced by the link: false."
  {:fsync? true
   :private? false
   :mode nil
   :symlink :replace
   :root nil
   :replace-file? false})

(def max-links
  "How many symbolic links `atomic-write!` follows in one chain with
   `:symlink :follow` before it refuses with ::too-many-links; the bound
   Linux puts on resolving one path."
  40)

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-input ::no-directory ::exists ::is-directory ::not-posix ::not-atomic
    ::too-many-links ::outside-root ::failed})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

(defn- options
  "opts merged over `defaults`, refused unless every key is in allowed
   and every key in flags is true or false."
  [opts allowed flags]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove allowed (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [k flags]
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
  ^"[Ljava.nio.file.attribute.FileAttribute;" [^Set perms]
  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute perms)]))

(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private follow (make-array LinkOption 0))

(defn- permissions-of
  "The POSIX permissions of p, not following a link, or nil when the
   file system has none."
  [^Path p]
  (try
    (Files/getPosixFilePermissions p no-follow)
    (catch UnsupportedOperationException _ nil)))

(defn- check-within!
  "Refuses, after deleting p, when p carries a permission granted does
   not hold. Runs before anything is written to p."
  [^Path p ^Set granted]
  (let [actual (permissions-of p)]
    (when-not (and actual (every? #(contains? granted %) actual))
      (Files/deleteIfExists p)
      (refuse! ::not-posix
               (str p " cannot be given its permissions: the file system "
                    (if actual
                      (str "gave it " (PosixFilePermissions/toString actual) " instead of "
                           (PosixFilePermissions/toString granted))
                      "has no POSIX permissions"))
               {:file (str p)}))))

(defn- unsupported-attrs!
  [^Path p ^Throwable e]
  (refuse! ::not-posix (str p " cannot be given its permissions: the file system has no POSIX permissions")
           {:file (str p)} e))

(defn- mode-of
  "The permission set a :mode option names, or nil for nil and :keep."
  ^Set [mode]
  (cond
    (or (nil? mode) (= :keep mode)) nil

    (string? mode)
    (try
      (PosixFilePermissions/fromString mode)
      (catch IllegalArgumentException e
        (refuse! ::invalid-options (str ":mode " (pr-str mode) " is not a permission string such as \"rwxr-xr-x\"")
                 {:option :mode} e)))

    (and (instance? Set mode) (every? #(instance? PosixFilePermission %) mode))
    (HashSet. ^java.util.Collection mode)

    :else
    (refuse! ::invalid-options ":mode must be nil, :keep, a permission string or a set of PosixFilePermission"
             {:option :mode})))

(defn- descriptor-path
  "The /proc/self/fd entry of an open channel on the file at p, found as
   the entry that is the same file as p, or nil when there is none: no
   /proc, or p no longer names the file the channel has open. A chmod of
   that entry changes the open file itself, whatever p names by then."
  [^Path p]
  (let [fds (fs/path "/proc/self/fd")]
    (when (Files/isDirectory fds follow)
      (some (fn [entry]
              (try
                (when (Files/isSameFile entry p) entry)
                (catch IOException _ nil)))
            (fs/list-dir fds)))))

(defn- set-mode!
  "Gives the staged file at p, which this process has open, exactly the
   permissions perms. A chmod by name follows a symbolic link (NOFOLLOW is
   not honoured on Linux), so a link planted at p would hand the mode to
   whatever it points at; the chmod goes through the open descriptor
   instead and is refused where that cannot be found."
  [^Path p ^Set perms]
  (let [entry (or (descriptor-path p)
                  (refuse! ::not-posix (str p " cannot be given " (PosixFilePermissions/toString perms)
                                            ": the umask removed some of them and the open file cannot be"
                                            " reached through /proc/self/fd to add them")
                           {:file (str p)}))]
    (try
      (Files/setAttribute entry "posix:permissions" perms follow)
      (catch UnsupportedOperationException e (unsupported-attrs! p e)))
    (let [actual (Files/getPosixFilePermissions entry follow)]
      (when-not (= (set perms) (set actual))
        (refuse! ::not-posix (str p " cannot be given " (PosixFilePermissions/toString perms)
                                  ": the file system keeps " (PosixFilePermissions/toString actual))
                 {:file (str p)})))))

;; Writing through a channel

(defn- open-new
  "A channel on a new file at p, created with CREATE_NEW, so an existing
   file or symbolic link at p is refused with
   FileAlreadyExistsException. With perms the file is created with them,
   less what the umask removes; without, with the umask's mode."
  ^FileChannel [^Path p ^Set perms]
  (let [options (doto (HashSet.)
                  (.add StandardOpenOption/CREATE_NEW)
                  (.add StandardOpenOption/WRITE)
                  (.add LinkOption/NOFOLLOW_LINKS))]
    (if perms
      (try
        (FileChannel/open p ^Set options (attrs perms))
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
   (let [{:keys [fsync?]} (options opts #{:fsync?} #{:fsync?})
         p (path-of path)
         bs (bytes-of content)
         perms (PosixFilePermissions/fromString private-file)
         channel (try
                   (open-new p perms)
                   (catch FileAlreadyExistsException e
                     (refuse! ::exists (str p " already exists") {:file (str p)} e))
                   (catch IOException e
                     (refuse! ::failed (str p " cannot be created: " (io-message e)) {:file (str p)} e)))]
     (try
       (with-open [^FileChannel channel channel]
         (check-within! p perms)
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
        parent (.getParent p)
        perms (PosixFilePermissions/fromString private-dir)]
    (try
      (when (and parent (not (Files/isDirectory parent follow)))
        (Files/createDirectories parent (attrs perms)))
      (Files/createDirectory p (attrs perms))
      (catch FileAlreadyExistsException e
        (refuse! ::exists (str p " already exists") {:file (str p)} e))
      (catch UnsupportedOperationException e
        (unsupported-attrs! p e))
      (catch IOException e
        (refuse! ::failed (str p " cannot be created: " (io-message e)) {:file (str p)} e)))
    (check-within! p perms)
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

(defn staged?
  "Whether path names a file or link `atomic-write!` or
   `replace-symlink!` is still preparing, or left behind when the process
   died before the rename. A caller that lists a directory it writes into
   skips these, and may delete those older than any write can take."
  [path]
  (boolean (re-matches #"\..+\.briko-[0-9a-f]{32}\.tmp" (str (.getFileName (fs/path path))))))

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
   planted file can cause.

   With perms the file is created with them less what the umask removes,
   so it never grants more than perms while it is written; the umask's
   removals are added back once the content is in and before the force,
   so the mode is on disk with the data."
  ^Path [^Path dir ^Path target content ^Set perms fsync?]
  (loop [attempt 0]
    (let [temp (.resolve dir ^String (temp-name (str (.getFileName target))))
          channel (try
                    (open-new temp perms)
                    (catch FileAlreadyExistsException e
                      (when (>= attempt 3)
                        (refuse! ::failed (str "no free temporary name beside " target) {:file (str target)} e))
                      nil))]
      (if-not channel
        (recur (inc attempt))
        (do
          (try
            (with-open [^FileChannel channel channel]
              (when perms (check-within! temp perms))
              (if (fn? content)
                (content channel)
                (write-all! channel (bytes-of content)))
              (when (and perms (not= (set perms) (set (permissions-of temp))))
                (set-mode! temp perms))
              (when fsync? (.force channel true)))
            ;; Whatever was thrown, the file this call created is removed first.
            #_{:splint/disable [lint/catch-throwable]}
            (catch Throwable e
              (Files/deleteIfExists temp)
              (throw e)))
          temp)))))

(defn- follow-links
  "The path of the file the chain of symbolic links at p ends at: its
   real directory and its name. Refuses past `max-links` links, a chain
   that ends in a directory or in a directory that does not exist, and
   with root, a file outside root."
  ^Path [^Path p root]
  (let [^Path end (loop [p p
                         n 0]
                    (if (Files/isSymbolicLink p)
                      (if (>= n (long max-links))
                        (refuse! ::too-many-links (str p " is reached through more than " max-links
                                                       " symbolic links, or through a loop")
                                 {:file (str p)})
                        (recur (.resolve (.getParent p) (Files/readSymbolicLink p)) (inc n)))
                      p))
        name (some-> (.getFileName end) str)]
    (when (or (nil? name) (= "." name) (= ".." name) (Files/isDirectory end no-follow))
      (refuse! ::is-directory (str p " leads to the directory " end) {:file (str p)}))
    (let [^Path dir (try
                      (.toRealPath (.getParent end) follow)
                      (catch IOException e
                        (refuse! ::no-directory (str "the directory of " end ", which " p " leads to, does not exist")
                                 {:file (str p)} e)))
          file (.resolve dir ^String name)]
      (when root
        (let [^Path real-root (try
                                (.toRealPath (path-of root) follow)
                                (catch IOException e
                                  (refuse! ::invalid-options (str ":root " root " does not exist") {:option :root} e)))]
          ;; Path.startsWith compares whole names, so /a/bc is not inside /a/b
          (when-not (and #_{:splint/disable [style/prefer-clj-string]} (.startsWith file ^Path real-root)
                         (not= file real-root))
            (refuse! ::outside-root (str p " leads to " file ", outside " real-root)
                     {:file (str p) :root (str real-root)}))))
      file)))

(defn- check-write-options!
  [{:keys [private? mode symlink root]}]
  (when-not (#{:replace :follow} symlink)
    (refuse! ::invalid-options ":symlink must be :replace or :follow" {:option :symlink}))
  (when (and root (not= :follow symlink))
    (refuse! ::invalid-options ":root needs :symlink :follow" {:option :root}))
  (when (and root (not (or (string? root) (instance? Path root) (instance? java.io.File root))))
    (refuse! ::invalid-options ":root must be a string, a Path or a File" {:option :root}))
  (when (and private? (some? mode))
    (refuse! ::invalid-options ":private? and :mode exclude each other; :private? is :mode \"rw-------\""
             {:option :mode}))
  (mode-of mode))

(defn atomic-write!
  "Replaces the file at path with content in one atomic rename and
   returns the path of the file written as a string. content is a string
   (UTF-8), a byte array, or a function of a java.nio.channels.FileChannel
   that writes the content itself, for content too large to hold in
   memory.

   The directory of path must exist. What is at path is replaced, a
   symbolic link included: the link is replaced by the file, its target
   is left alone. With `:symlink :follow` the link chain at path is
   followed instead, up to `max-links` links, and the file it ends at is
   replaced, staged beside that file; the links stay as they are, the
   returned path is the file written, and a chain ending at a name that
   does not exist yet creates the file there, as writing through the link
   would. :root confines where that file may lie.

   The new file gets the mode a new file gets under the umask, or
   rw------- with :private?, or exactly :mode, set on the staged file
   before the rename, so the path never shows the file with another mode.
   `:mode :keep` copies the permissions of the regular file being
   replaced, or uses the umask's mode when there is none (nothing at the
   path, or a link being replaced). Owner and group are always the
   writer's; setuid, setgid and sticky bits never carry over.

   Refuses with ::not-atomic when the file system cannot rename
   atomically, ::no-directory when the directory is missing, ::not-posix
   when :private? or :mode is asked of a file system that cannot keep the
   permissions, ::too-many-links, ::is-directory and ::outside-root when
   a followed chain is too long, ends at a directory or leaves :root, and
   ::failed when writing or renaming fails. In every case, and when a
   content function throws, the temporary file is removed and the file at
   path is untouched. A content function's own exception is rethrown as
   it is. Options :fsync?, :private?, :mode, :symlink and :root (see
   `defaults`)."
  ([path content] (atomic-write! path content nil))
  ([path content opts]
   (let [{:keys [fsync? private? mode symlink root] :as opts}
         (options opts #{:fsync? :private? :mode :symlink :root} #{:fsync? :private?})
         perms (check-write-options! opts)
         given (path-of path)]
     (when-not (or (fn? content) (string? content) (bytes? content))
       (refuse! ::invalid-input "the content must be a string, a byte array or a function of a FileChannel" {}))
     (when-not (and (.getParent given) (Files/isDirectory (.getParent given) follow))
       (refuse! ::no-directory (str "the directory of " given " does not exist") {:file (str given)}))
     (let [target (if (= :follow symlink) (follow-links given root) given)
           dir (.getParent target)
           perms (cond
                   private? (PosixFilePermissions/fromString private-file)
                   (= :keep mode) (when (Files/isRegularFile target no-follow) (permissions-of target))
                   :else perms)
           temp (try
                  (write-temp! dir target content perms fsync?)
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

;; Symbolic links

(defn- link-target-of
  ^Path [target]
  (when-not (or (and (string? target) (not (str/blank? target))) (instance? Path target)
                (instance? java.io.File target))
    (refuse! ::invalid-input "the link target must be a non-blank string, a Path or a File" {}))
  (try
    (fs/path target)
    ;; InvalidPathException, which babashka does not expose
    (catch IllegalArgumentException e
      (refuse! ::invalid-input (str "the link target is not a path: " (ex-message e)) {} e))))

(defn replace-symlink!
  "Points the symbolic link at path to target in one atomic rename and
   returns path as a string: a reader of path finds the old link or the
   new one, never nothing. target is stored as given, so a relative
   target stays relative to the link's directory, and it need not exist:
   a dangling link is created as `ln -s` would, and a dangling link at
   path is replaced like any other.

   The new link is created beside path under a unique name and renamed
   over it. What is at path is replaced when it is a symbolic link,
   whatever it points at, a directory included; a regular file is
   replaced only with :replace-file?, and refused with ::exists
   otherwise. A directory at path (not a link to one) is refused with
   ::is-directory; the rename would fail on it in any case, so a
   directory that appears after the check is refused as ::failed.

   Refuses with ::no-directory when the directory of path is missing,
   ::not-atomic when the file system cannot rename atomically and
   ::failed when creating or renaming fails, removing the staged link.
   Options :fsync?, which forces the directory after the rename, and
   :replace-file? (see `defaults`)."
  ([path target] (replace-symlink! path target nil))
  ([path target opts]
   (let [{:keys [fsync? replace-file?]} (options opts #{:fsync? :replace-file?} #{:fsync? :replace-file?})
         p (path-of path)
         dir (.getParent p)
         link-target (link-target-of target)]
     (when-not (and dir (Files/isDirectory dir follow))
       (refuse! ::no-directory (str "the directory of " p " does not exist") {:file (str p)}))
     (cond
       (Files/isDirectory p no-follow)
       (refuse! ::is-directory (str p " is a directory") {:file (str p)})

       (and (Files/isRegularFile p no-follow) (not replace-file?))
       (refuse! ::exists (str p " is a regular file; pass :replace-file? to replace it") {:file (str p)}))
     (let [staged (loop [attempt 0]
                    (let [staged (.resolve dir ^String (temp-name (str (.getFileName p))))
                          created (try
                                    (Files/createSymbolicLink staged link-target (make-array FileAttribute 0))
                                    (catch FileAlreadyExistsException e
                                      (when (>= attempt 3)
                                        (refuse! ::failed (str "no free temporary name beside " p) {:file (str p)} e))
                                      nil)
                                    (catch IOException e
                                      (refuse! ::failed (str p " cannot be created: " (io-message e))
                                               {:file (str p)} e)))]
                      (if created staged (recur (inc attempt)))))]
       (try
         (move! staged p)
         #_{:splint/disable [lint/catch-throwable]}
         (catch Throwable e
           (Files/deleteIfExists staged)
           (cond
             (atomic-move-unsupported? e)
             (refuse! ::not-atomic (str p " cannot be replaced atomically on this file system") {:file (str p)} e)

             (instance? IOException e)
             (refuse! ::failed (str p " cannot be replaced: " (io-message e)) {:file (str p)} e)

             :else (throw e))))
       (when fsync?
         (try
           (force-directory! dir)
           (catch IOException e
             (refuse! ::failed (str "the directory of " p " cannot be forced to disk, so the new link"
                                    " is in place but may not survive a crash: " (io-message e))
                      {:file (str p)} e))))
       (str p)))))

(defn permissions
  "The POSIX permissions of path as a string such as \"rw-------\", not
   following a symbolic link, or nil on a file system without them."
  [path]
  (some-> (permissions-of (path-of path)) PosixFilePermissions/toString))
