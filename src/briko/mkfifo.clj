;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.mkfifo
  "Creates owner-only named pipes in this process through mkfifo(3), for
   briko.process on the JVM. Requires the foreign function API (Java 22
   or later) and is loaded only through `maker`'s caller, never under
   babashka, which has no such API.

   The JVM prints a warning on the first restricted call by code without
   native access, and later releases will refuse it, so `maker` answers
   nil unless native access is enabled for our code, for example with
   --enable-native-access=ALL-UNNAMED. briko.process then runs the mkfifo
   program instead."
  (:import (java.lang.foreign Arena FunctionDescriptor Linker Linker$Option MemoryLayout
                              MemoryLayout$PathElement MemorySegment StructLayout ValueLayout)
           (java.lang.invoke MethodHandle)
           (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util List)))

(set! *warn-on-reflection* true)

(def ^:private fifo-rw-owner
  "The st_mode of a named pipe readable and writable by its owner alone:
   S_IFIFO | 0600."
  (bit-or 8r10000 8r600))

(defn- native-access?
  []
  (.isNativeAccessEnabled (.getModule (class native-access?))))

(defn- downcall
  "A handle on mkfifo(3) that captures errno, or nil where the C library
   has none."
  ^MethodHandle [^Linker linker]
  (when-let [^MemorySegment symbol (.orElse (.find (.defaultLookup linker) "mkfifo") nil)]
    (.downcallHandle linker
                     symbol
                     (FunctionDescriptor/of ValueLayout/JAVA_INT
                                            (into-array MemoryLayout [ValueLayout/ADDRESS ValueLayout/JAVA_INT]))
                     ^"[Ljava.lang.foreign.Linker$Option;"
                     (into-array Linker$Option [(Linker$Option/captureCallState (into-array String ["errno"]))]))))

(defn- owner-only!
  "Sets the pipe at path to rw------- whatever the umask removed, and
   answers nil once it is a named pipe with exactly that mode, or the
   reason it is not."
  [^String path]
  (let [p (Path/of path (make-array String 0))]
    (Files/setPosixFilePermissions p (PosixFilePermissions/fromString "rw-------"))
    (let [mode (Files/getAttribute p "unix:mode" (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))]
      (when-not (= fifo-rw-owner mode)
        (str "mkfifo(3) made " path " with mode " (Integer/toOctalString (int mode)))))))

(defn- creator
  "A function creating an owner-only named pipe at a path through mh,
   answering nil, or the reason it could not."
  [^MethodHandle mh]
  (let [^StructLayout state-layout (Linker$Option/captureStateLayout)
        errno-offset (.byteOffset state-layout
                                  ^"[Ljava.lang.foreign.MemoryLayout$PathElement;"
                                  (into-array MemoryLayout$PathElement [(MemoryLayout$PathElement/groupElement "errno")]))]
    (fn [^String path]
      (try
        (with-open [arena (Arena/ofConfined)]
          (let [state (.allocate arena state-layout)
                rc (.invokeWithArguments mh ^List (list state (.allocateFrom arena path) (int 8r600)))]
            (if (zero? (long rc))
              (owner-only! path)
              (str "mkfifo(3) failed for " path " with errno "
                   (.get ^MemorySegment state ValueLayout/JAVA_INT (long errno-offset))))))
        (catch Exception e
          (str "mkfifo(3) failed for " path ": " (or (ex-message e) (.getName (class e)))))))))

(defn- works?
  "Whether make! creates an owner-only named pipe in a fresh directory,
   so a runtime that links the call but cannot make it is found before
   any program depends on it."
  [make!]
  (let [dir (Files/createTempDirectory "briko-mkfifo-" (make-array FileAttribute 0))
        fifo (.resolve dir "probe")]
    (try
      (and (nil? (make! (str fifo)))
           (Files/exists fifo (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
      (finally
        (Files/deleteIfExists fifo)
        (Files/deleteIfExists dir)))))

(defn maker
  "A function of one path that creates a named pipe there, readable and
   writable by the owner only, and answers nil, or the reason it could
   not. nil where native access is not enabled for our code, the C
   library has no mkfifo, or a probe pipe could not be made."
  []
  (when (native-access?)
    (when-let [mh (downcall (Linker/nativeLinker))]
      (let [make! (creator mh)]
        (when (works? make!)
          make!)))))
