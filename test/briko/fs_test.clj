;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.fs-test
  (:require [babashka.fs :as fs]
            [briko.fs :as bfs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.io IOException)
           (java.nio ByteBuffer)
           (java.nio.file CopyOption Files LinkOption StandardCopyOption StandardOpenOption)
           (java.nio.file.attribute PosixFilePermission PosixFilePermissions)
           (java.nio.channels FileChannel)
           (java.util.concurrent CountDownLatch)))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (bfs/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- holds?
  [num-tests property]
  (let [result (tc/quick-check num-tests property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :fail :shrunk])))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-fs-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- names
  "The names in dir, sorted."
  [dir]
  (sort (map (comp str fs/file-name) (fs/list-dir dir))))

(defn- write-channel
  [^FileChannel channel ^String s]
  (.write channel (ByteBuffer/wrap (.getBytes s "UTF-8"))))

;; atomic-write!

(deftest atomic-write-creates-and-replaces
  (with-temp-dir [dir]
    (let [f (str dir "/a.edn")]
      (is (= f (bfs/atomic-write! f "one")))
      (is (= "one" (slurp f)))
      (bfs/atomic-write! f (.getBytes "two" "UTF-8"))
      (is (= "two" (slurp f)))
      (bfs/atomic-write! f "drei ü" {:fsync? false})
      (is (= "drei ü" (slurp f)))
      (is (= ["a.edn"] (names dir))))))

(deftest atomic-write-takes-a-channel-function
  (with-temp-dir [dir]
    (let [f (str dir "/big")]
      (bfs/atomic-write! f (fn [channel] (dotimes [_ 1000] (write-channel channel "0123456789"))))
      (is (= 10000 (fs/size f))))))

(deftest atomic-write-gives-the-umask-mode-or-private
  (with-temp-dir [dir]
    (let [plain (str dir "/plain")
          private (str dir "/private")]
      (bfs/atomic-write! plain "x")
      (bfs/atomic-write! private "x" {:private? true})
      (is (= "rw-------" (bfs/permissions private)))
      ;; A new file under any umask grants its owner read and write and
      ;; never execute; the temp-file default of rw------- is not forced.
      (is (str/starts-with? (bfs/permissions plain) "rw-"))
      (is (= (bfs/permissions plain)
             (do (spit (str dir "/spit") "x") (bfs/permissions (str dir "/spit"))))))))

(deftest atomic-write-replaces-a-private-file-with-a-plain-one-only-when-asked
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (bfs/atomic-write! f "x" {:private? true})
      (bfs/atomic-write! f "y" {:private? true})
      (is (= "rw-------" (bfs/permissions f))))))

(deftest atomic-write-replaces-a-link-and-leaves-its-target
  (with-temp-dir [dir]
    (let [target (str dir "/target")
          link (str dir "/link")]
      (spit target "kept")
      (fs/create-sym-link link target)
      (bfs/atomic-write! link "new")
      (is (= "kept" (slurp target)))
      (is (not (fs/sym-link? link)))
      (is (= "new" (slurp link))))))

(deftest a-failing-content-function-leaves-the-target-and-no-temp-file
  (with-temp-dir [dir]
    (let [f (str dir "/f")
          boom (ex-info "boom" {:boom true})]
      (spit f "old")
      (is (= boom (try
                    (bfs/atomic-write! f (fn [channel]
                                           (write-channel channel "half written")
                                           (throw boom)))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))))
      (is (= "old" (slurp f)))
      (is (= ["f"] (names dir)))
      (testing "an IOException from the content function"
        (is (= ::bfs/failed (refusal #(bfs/atomic-write! f (fn [_] (throw (IOException. "disk full")))))))
        (is (= "old" (slurp f)))
        (is (= ["f"] (names dir)))))))

(defn- cross-device-move!
  "Moves from to a directory on another file system with ATOMIC_MOVE,
   which the JDK refuses with AtomicMoveNotSupportedException, or returns
   :same-device when /dev/shm is missing or on the same file system."
  [from]
  (let [other (str "/dev/shm/briko-fs-test-" (random-uuid))]
    (if-not (fs/directory? "/dev/shm")
      :same-device
      (try
        (Files/move (fs/path from) (fs/path other) (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
        (fs/move other from)
        :same-device
        (finally (fs/delete-if-exists other))))))

(deftest a-real-cross-device-atomic-move-is-recognised
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (spit f "x")
      (let [outcome (try (cross-device-move! f) (catch Exception e e))]
        (when-not (= :same-device outcome)
          (is (#'briko.fs/atomic-move-unsupported? outcome) (str (class outcome))))
        (is (not (#'briko.fs/atomic-move-unsupported? (IOException. "x"))))))))

(deftest a-file-system-without-atomic-rename-is-refused-not-copied
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (spit f "old")
      (with-redefs-fn {#'briko.fs/move! (fn [from _]
                                          (when (= :same-device (cross-device-move! from))
                                            (throw (ex-info "no second file system" {}))))}
        (fn []
          (let [outcome (refusal #(bfs/atomic-write! f "new"))]
            (when-not (= [:foreign "no second file system"] outcome)
              (is (= ::bfs/not-atomic outcome))))))
      (with-redefs-fn {#'briko.fs/move! (fn [_ _] (throw (IOException. "EIO")))}
        (fn [] (is (= ::bfs/failed (refusal #(bfs/atomic-write! f "new"))))))
      (is (= "old" (slurp f)))
      (is (= ["f"] (names dir))))))

(deftest atomic-write-refuses-a-missing-directory-and-a-directory-target
  (with-temp-dir [dir]
    (is (= ::bfs/no-directory (refusal #(bfs/atomic-write! (str dir "/no/f") "x"))))
    (fs/create-dirs (str dir "/sub/inner"))
    (is (= ::bfs/failed (refusal #(bfs/atomic-write! (str dir "/sub") "x"))))
    (is (= ["sub"] (names dir)))))

(deftest atomic-write-checks-its-arguments
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (is (= ::bfs/invalid-input (refusal #(bfs/atomic-write! nil "x"))))
      (is (= ::bfs/invalid-input (refusal #(bfs/atomic-write! "" "x"))))
      (is (= ::bfs/invalid-input (refusal #(bfs/atomic-write! f 42))))
      (is (= ::bfs/invalid-input (refusal #(bfs/atomic-write! f nil))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:fsync true}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:fsync? "yes"}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/write-private! f "x" {:private? true}))))
      (is (= [] (names dir))))))

(deftest fsync-forces-the-file-and-the-directory
  (with-temp-dir [dir]
    (let [forced (atom [])
          f (str dir "/f")]
      (with-redefs-fn {#'briko.fs/force-directory! (fn [d] (swap! forced conj (str d)))}
        (fn []
          (bfs/atomic-write! f "x")
          (is (= [dir] @forced))
          (bfs/atomic-write! f "y" {:fsync? false})
          (is (= [dir] @forced))))
      (testing "the real directory force succeeds on this file system"
        (is (= f (bfs/atomic-write! f "z" {:fsync? true})))))))

(deftest a-failing-directory-force-is-reported
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (with-redefs-fn {#'briko.fs/force-directory! (fn [_] (throw (IOException. "EIO")))}
        (fn [] (is (= ::bfs/failed (refusal #(bfs/atomic-write! f "x")))))))))

(deftest temp-names-do-not-repeat
  (let [ns (repeatedly 20000 #(#'briko.fs/temp-name "f"))]
    (is (= 20000 (count (set ns))))
    (is (every? #(re-matches #"\.f\.briko-[0-9a-f]{32}\.tmp" %) ns))))

(deftest a-planted-temp-name-is-skipped
  (with-temp-dir [dir]
    (let [planted (str dir "/.f.briko-planted.tmp")
          calls (atom 0)
          real @#'briko.fs/temp-name]
      (spit planted "planted")
      (with-redefs-fn {#'briko.fs/temp-name (fn [n] (if (= 1 (swap! calls inc)) ".f.briko-planted.tmp" (real n)))}
        (fn [] (bfs/atomic-write! (str dir "/f") "x")))
      (is (= "planted" (slurp planted)))
      (is (= "x" (slurp (str dir "/f"))))
      (is (= 2 @calls)))))

(deftest two-writers-to-one-path-never-interleave
  (with-temp-dir [dir]
    (let [f (str dir "/f")
          a (str/join (repeat 200000 "a"))
          b (str/join (repeat 300000 "b"))
          start (CountDownLatch. 1)
          seen (atom #{})
          writer (fn [content]
                   (future
                     (.await start)
                     (dotimes [_ 40] (bfs/atomic-write! f content {:fsync? false}))))
          reader (future
                   (.await start)
                   (dotimes [_ 400]
                     (when (fs/exists? f)
                       (let [s (slurp f)]
                         (swap! seen conj (cond (= s a) :a (= s b) :b :else [:torn (count s)]))))))
          workers [(writer a) (writer b)]]
      (spit f a)
      (.countDown start)
      (run! deref workers)
      @reader
      (is (contains? #{a b} (slurp f)))
      (is (every? #{:a :b} @seen) (pr-str (remove #{:a :b} @seen)))
      (is (= ["f"] (names dir))))))

(deftest property-atomic-write-round-trips-bytes
  (with-temp-dir [dir]
    (holds? 100
            (prop/for-all [bs gen/bytes
                           private? gen/boolean]
              (let [f (str dir "/p")]
                (bfs/atomic-write! f bs {:private? private? :fsync? false})
                (and (= (seq bs) (seq (fs/read-all-bytes f)))
                     (= ["p"] (names dir))))))))

;; write-private!

(deftest write-private-creates-an-owner-only-file
  (with-temp-dir [dir]
    (let [f (str dir "/secret")]
      (is (= f (bfs/write-private! f "dummy")))
      (is (= "dummy" (slurp f)))
      (is (= "rw-------" (bfs/permissions f)))
      (bfs/write-private! (str dir "/bytes") (.getBytes "b" "UTF-8") {:fsync? false})
      (is (= "b" (slurp (str dir "/bytes")))))))

(deftest write-private-refuses-an-existing-file-and-a-link
  (with-temp-dir [dir]
    (let [f (str dir "/f")
          target (str dir "/target")
          link (str dir "/link")
          dangling (str dir "/dangling")]
      (spit f "planted")
      (spit target "kept")
      (fs/create-sym-link link target)
      (fs/create-sym-link dangling (str dir "/nowhere"))
      (is (= ::bfs/exists (refusal #(bfs/write-private! f "x"))))
      (is (= ::bfs/exists (refusal #(bfs/write-private! link "x"))))
      (is (= ::bfs/exists (refusal #(bfs/write-private! dangling "x"))))
      (is (= "planted" (slurp f)))
      (is (= "kept" (slurp target)))
      (is (not (fs/exists? (str dir "/nowhere")))))))

(deftest write-private-refuses-a-missing-directory
  (with-temp-dir [dir]
    (is (= ::bfs/failed (refusal #(bfs/write-private! (str dir "/no/f") "x"))))))

(deftest a-file-system-that-ignores-the-mode-is-refused-and-cleaned-up
  (with-temp-dir [dir]
    (let [wide (java.nio.file.attribute.PosixFilePermissions/fromString "rw-r--r--")]
      (with-redefs-fn {#'briko.fs/permissions-of (constantly wide)}
        (fn []
          (is (= ::bfs/not-posix (refusal #(bfs/write-private! (str dir "/f") "dummy"))))
          (is (= ::bfs/not-posix (refusal #(bfs/atomic-write! (str dir "/g") "dummy" {:private? true}))))
          (is (= ::bfs/not-posix (refusal #(bfs/create-private-directory! (str dir "/d")))))
          (testing "a plain atomic write does not check"
            (is (string? (bfs/atomic-write! (str dir "/plain") "x"))))))
      (with-redefs-fn {#'briko.fs/permissions-of (constantly nil)}
        (fn [] (is (= ::bfs/not-posix (refusal #(bfs/write-private! (str dir "/f") "dummy"))))))
      (is (= ["plain"] (names dir))))))

;; create-private-directory!

(deftest create-private-directory-creates-it-and-its-parents-owner-only
  (with-temp-dir [dir]
    (let [d (str dir "/a/b/c")]
      (is (= d (bfs/create-private-directory! d)))
      (is (= "rwx------" (bfs/permissions d)))
      (is (= "rwx------" (bfs/permissions (str dir "/a"))))
      (is (= "rwx------" (bfs/permissions (str dir "/a/b")))))))

(deftest create-private-directory-refuses-anything-existing
  (with-temp-dir [dir]
    (let [existing (str dir "/d")
          file (str dir "/f")
          link (str dir "/link")]
      (fs/create-dir existing)
      (spit file "x")
      (fs/create-sym-link link existing)
      (is (= ::bfs/exists (refusal #(bfs/create-private-directory! existing))))
      (is (= ::bfs/exists (refusal #(bfs/create-private-directory! file))))
      (is (= ::bfs/exists (refusal #(bfs/create-private-directory! link))))
      (is (fs/sym-link? link)))))

(deftest permissions-of-a-missing-file-throws
  (is (thrown? IOException (bfs/permissions "/nonexistent/briko"))))

;; atomic-write! with :mode

(deftest atomic-write-sets-exactly-the-mode-it-is-given
  (with-temp-dir [dir]
    (doseq [mode ["rwxr-xr-x" "rw-rw-rw-" "rwxrwxrwx" "r--------" "rw-r-----"]]
      (let [f (str dir "/f")]
        (is (= f (bfs/atomic-write! f "x" {:mode mode})))
        (is (= mode (bfs/permissions f)))
        (is (= "x" (slurp f)))))
    (testing "a set of PosixFilePermission"
      (bfs/atomic-write! (str dir "/s") "x" {:mode (PosixFilePermissions/fromString "rwxr-x---")})
      (bfs/atomic-write! (str dir "/t") "x" {:mode #{PosixFilePermission/OWNER_READ}})
      (is (= "rwxr-x---" (bfs/permissions (str dir "/s"))))
      (is (= "r--------" (bfs/permissions (str dir "/t")))))
    (is (= ["f" "s" "t"] (names dir)))))

(deftest the-staged-file-has-its-mode-before-the-rename
  (with-temp-dir [dir]
    (let [f (str dir "/f")
          at-rename (atom [])
          real @#'briko.fs/move!]
      (with-redefs-fn {#'briko.fs/move! (fn [from to]
                                          (swap! at-rename conj [(bfs/permissions from) (slurp (str from))])
                                          (real from to))}
        (fn []
          ;; rw-rw-rw- is wider than the umask lets a new file be under 022
          ;; or 077, so the mode has to be added after creation
          (bfs/atomic-write! f "one" {:mode "rw-rw-rw-"})
          (bfs/atomic-write! f "two" {:mode "rwxr-x---" :fsync? false})))
      (is (= [["rw-rw-rw-" "one"] ["rwxr-x---" "two"]] @at-rename)))))

(defn- watch-modes
  "Writes f n times with write!, while another thread reads the
   permissions of f only, and returns every mode it saw."
  [f n write!]
  (let [start (CountDownLatch. 1)
        done (atom false)
        seen (atom #{})
        watcher (future
                  (.await start)
                  (while (not @done)
                    (when-let [p (try (bfs/permissions f) (catch IOException _ nil))]
                      (swap! seen conj p)))
                  ;; one read after the last write, however the threads ran
                  (swap! seen conj (bfs/permissions f)))]
    (.countDown start)
    (try
      (dotimes [_ n] (write!))
      (finally (reset! done true)))
    @watcher
    @seen))

(deftest a-reader-of-the-target-never-sees-another-mode
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (bfs/atomic-write! f "x" {:mode "rw-rw-r--"})
      (is (= #{"rw-rw-r--"}
             (watch-modes f 300 #(bfs/atomic-write! f "x" {:mode "rw-rw-r--" :fsync? false})))))
    (testing "negative control: the watcher sees a mode that is wrong"
      (let [f (str dir "/control")]
        (bfs/atomic-write! f "x" {:mode "rw-rw-r--"})
        (with-redefs-fn {#'briko.fs/set-mode! (fn [_ _] nil)}
          (fn []
            (is (contains? (watch-modes f 50 #(bfs/atomic-write! f "x" {:mode "rw-rw-r--" :fsync? false}))
                           "rw-r--r--"))))))))

(deftest mode-keep-copies-the-mode-of-the-file-it-replaces
  (with-temp-dir [dir]
    (let [f (str dir "/f")
          fresh (str dir "/fresh")
          spat (str dir "/spat")]
      (bfs/atomic-write! f "old" {:mode "rwxr-x--x"})
      (bfs/atomic-write! f "new" {:mode :keep})
      (is (= "rwxr-x--x" (bfs/permissions f)))
      (is (= "new" (slurp f)))
      (testing "nothing to keep: the umask's mode"
        (spit spat "x")
        (bfs/atomic-write! fresh "x" {:mode :keep})
        (is (= (bfs/permissions spat) (bfs/permissions fresh))))
      (testing "a link being replaced has no mode to keep; its target keeps its own"
        (let [target (str dir "/target")
              link (str dir "/link")]
          (bfs/atomic-write! target "t" {:mode "rwx------"})
          (fs/create-sym-link link "target")
          (bfs/atomic-write! link "l" {:mode :keep})
          (is (not (fs/sym-link? link)))
          (is (= (bfs/permissions spat) (bfs/permissions link)))
          (is (= "rwx------" (bfs/permissions target)))
          (is (= "t" (slurp target))))))))

(deftest mode-options-are-checked
  (with-temp-dir [dir]
    (let [f (str dir "/f")]
      (doseq [bad ["rwx" "rwxrwxrwz" "755" 0755 #{:owner-read} #{"OWNER_READ"} :preserve]]
        (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:mode bad}))) (pr-str bad)))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:mode "rw-------" :private? true}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:mode :keep :private? true}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:symlink :follows}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:root dir}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/atomic-write! f "x" {:symlink :follow :root 42}))))
      (is (= ::bfs/invalid-options (refusal #(bfs/write-private! f "x" {:mode "rw-------"}))))
      (testing "positive control: :private? false beside a mode is fine"
        (is (= f (bfs/atomic-write! f "x" {:mode "rw-------" :private? false}))))
      (is (= ["f"] (names dir))))))

(deftest a-mode-on-a-file-system-without-posix-permissions-is-refused
  ;; Every file system a test can reach here keeps POSIX permissions
  ;; (/tmp and /dev/shm both do), so the file system's answers are
  ;; replaced: no permissions at all, and a mode it does not keep.
  (with-temp-dir [dir]
    (spit (str dir "/f") "old")
    (with-redefs-fn {#'briko.fs/permissions-of (constantly nil)}
      (fn [] (is (= ::bfs/not-posix (refusal #(bfs/atomic-write! (str dir "/f") "new" {:mode "rwxr-xr-x"}))))))
    (with-redefs-fn {#'briko.fs/attrs (fn [_] (throw (UnsupportedOperationException. "no posix")))}
      (fn [] (is (= ::bfs/not-posix (refusal #(bfs/atomic-write! (str dir "/f") "new" {:mode "rwxr-xr-x"}))))))
    ;; a file system that accepts the chmod and keeps another mode, as
    ;; set-mode! reports it after reading the mode back
    (with-redefs-fn {#'briko.fs/set-mode! (fn [p _] (throw (ex-info "kept" {:type ::bfs/not-posix :file (str p)})))}
      (fn [] (is (= ::bfs/not-posix (refusal #(bfs/atomic-write! (str dir "/f") "new" {:mode "rw-rw-rw-"}))))))
    (is (= "old" (slurp (str dir "/f"))))
    (is (= ["f"] (names dir)))))

(deftest without-proc-a-mode-the-umask-narrows-is-refused
  (with-temp-dir [dir]
    (with-redefs-fn {#'briko.fs/descriptor-path (constantly nil)}
      (fn []
        (is (= ::bfs/not-posix (refusal #(bfs/atomic-write! (str dir "/w") "x" {:mode "rw-rw-rw-"}))))
        (testing "a mode the umask leaves alone needs no chmod"
          (is (string? (bfs/atomic-write! (str dir "/p") "x" {:mode "rw-------"})))
          (is (= "rw-------" (bfs/permissions (str dir "/p")))))))
    (is (= ["p"] (names dir)))))

(deftest the-chmod-never-follows-a-link-planted-at-the-staged-name
  (with-temp-dir [dir]
    (let [staged (str dir "/staged")
          moved (str dir "/moved")
          victim (str dir "/victim")
          wide (PosixFilePermissions/fromString "rw-rw-rw-")]
      (bfs/atomic-write! victim "secret" {:mode "rw-------"})
      (with-open [_ (FileChannel/open (fs/path staged)
                                      (into-array StandardOpenOption [StandardOpenOption/CREATE_NEW
                                                                      StandardOpenOption/WRITE]))]
        (fs/move staged moved)
        (fs/create-sym-link staged victim)
        (is (= ::bfs/not-posix (refusal #(#'briko.fs/set-mode! (fs/path staged) wide))))
        (is (= "rw-------" (bfs/permissions victim))))
      (testing "negative control: a chmod by name with NOFOLLOW_LINKS follows the link"
        (Files/setAttribute (fs/path staged) "posix:permissions" wide
                            (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
        (is (= "rw-rw-rw-" (bfs/permissions victim)))))))

;; atomic-write! with :symlink :follow

(deftest following-a-link-writes-its-target-and-keeps-the-link
  (with-temp-dir [dir]
    (let [target (str dir "/target")
          link (str dir "/link")]
      (spit target "old")
      (fs/create-sym-link link "target")
      (is (= (str (fs/real-path target)) (bfs/atomic-write! link "new" {:symlink :follow})))
      (is (fs/sym-link? link))
      (is (= "target" (str (fs/read-link link))))
      (is (= "new" (slurp target)))
      (testing "with no link at the path it is a plain write"
        (is (= (str dir "/plain") (bfs/atomic-write! (str dir "/plain") "p" {:symlink :follow})))))))

(deftest following-a-chain-stages-beside-the-file-it-ends-at
  (with-temp-dir [dir]
    (let [home (str dir "/home")
          src (str dir "/src/dots")
          from-dirs (atom [])
          real @#'briko.fs/move!]
      (fs/create-dirs home)
      (fs/create-dirs src)
      (spit (str src "/rc") "old")
      ;; home/.rc -> home/.rc2 (relative) -> src/dots/rc (absolute)
      (fs/create-sym-link (str src "/../dots/rc.link") "rc")
      (fs/create-sym-link (str home "/.rc2") (str src "/../dots/rc.link"))
      (fs/create-sym-link (str home "/.rc") ".rc2")
      (with-redefs-fn {#'briko.fs/move! (fn [from to]
                                          (swap! from-dirs conj (str (fs/parent from)))
                                          (real from to))}
        (fn [] (bfs/atomic-write! (str home "/.rc") "new" {:symlink :follow :mode :keep})))
      (is (= "new" (slurp (str src "/rc"))))
      (is (= [(str (fs/real-path src))] @from-dirs))
      (is (every? fs/sym-link? [(str home "/.rc") (str home "/.rc2") (str src "/rc.link")]))
      (is (= [".rc" ".rc2"] (names home))))))

(deftest following-keeps-the-mode-of-the-file-it-ends-at
  (with-temp-dir [dir]
    (let [target (str dir "/target")
          link (str dir "/link")]
      (bfs/atomic-write! target "old" {:mode "rwx--x--x"})
      (fs/create-sym-link link "target")
      (bfs/atomic-write! link "new" {:symlink :follow :mode :keep})
      (is (= "rwx--x--x" (bfs/permissions target))))))

(deftest following-a-dangling-link-creates-the-file-it-names
  (with-temp-dir [dir]
    (let [link (str dir "/link")]
      (fs/create-sym-link link "missing")
      (bfs/atomic-write! link "new" {:symlink :follow})
      (is (= "new" (slurp (str dir "/missing"))))
      (is (fs/sym-link? link))
      (testing "but not a missing directory"
        (fs/create-sym-link (str dir "/deep") "no/such/file")
        (is (= ::bfs/no-directory (refusal #(bfs/atomic-write! (str dir "/deep") "x" {:symlink :follow}))))
        (is (not (fs/exists? (str dir "/no"))))))))

(deftest following-refuses-loops-and-long-chains
  (with-temp-dir [dir]
    (fs/create-sym-link (str dir "/a") "b")
    (fs/create-sym-link (str dir "/b") "a")
    (is (= ::bfs/too-many-links (refusal #(bfs/atomic-write! (str dir "/a") "x" {:symlink :follow}))))
    (fs/create-sym-link (str dir "/self") "self")
    (is (= ::bfs/too-many-links (refusal #(bfs/atomic-write! (str dir "/self") "x" {:symlink :follow}))))
    (let [chain (fn [prefix n]
                  ;; prefix0 -> prefix1 -> ... -> prefix<n>, a regular file
                  (spit (str dir "/" prefix n) "end")
                  (doseq [i (range n)]
                    (fs/create-sym-link (str dir "/" prefix i) (str prefix (inc i))))
                  (str dir "/" prefix 0))]
      (testing "a chain of max-links links is followed"
        (bfs/atomic-write! (chain "ok" bfs/max-links) "x" {:symlink :follow})
        (is (= "x" (slurp (str dir "/ok" bfs/max-links)))))
      (testing "one more is refused"
        (let [start (chain "long" (inc bfs/max-links))]
          (is (= ::bfs/too-many-links (refusal #(bfs/atomic-write! start "x" {:symlink :follow}))))
          (is (= "end" (slurp (str dir "/long" (inc bfs/max-links))))))))
    (is (= "a" (str (fs/read-link (str dir "/b")))))))

(deftest following-to-a-directory-is-refused
  (with-temp-dir [dir]
    (fs/create-dir (str dir "/d"))
    (fs/create-sym-link (str dir "/link") "d")
    (fs/create-sym-link (str dir "/dots") "..")
    (is (= ::bfs/is-directory (refusal #(bfs/atomic-write! (str dir "/link") "x" {:symlink :follow}))))
    (is (= ::bfs/is-directory (refusal #(bfs/atomic-write! (str dir "/dots") "x" {:symlink :follow}))))
    (is (= [] (names (str dir "/d"))))))

(deftest following-stays-inside-the-root
  (with-temp-dir [dir]
    (let [root (str dir "/root")
          outside (str dir "/outside")]
      (fs/create-dirs (str root "/sub"))
      (spit outside "kept")
      (spit (str root "/sub/in") "old")
      (fs/create-sym-link (str root "/in") "sub/in")
      (fs/create-sym-link (str root "/out") "../outside")
      (fs/create-sym-link (str root "/abs") outside)
      (fs/create-sym-link (str root "/sub/up") "../../outside")
      (fs/create-sym-link (str root "/itself") ".")
      ;; a sibling whose name starts with the root's
      (fs/create-dirs (str dir "/rootx"))
      (fs/create-sym-link (str root "/sibling") "../rootx/f")
      (is (string? (bfs/atomic-write! (str root "/in") "new" {:symlink :follow :root root})))
      (is (= "new" (slurp (str root "/sub/in"))))
      (doseq [l ["out" "abs" "sub/up" "sibling"]]
        (is (= ::bfs/outside-root (refusal #(bfs/atomic-write! (str root "/" l) "x" {:symlink :follow :root root})))
            l))
      (is (= ::bfs/is-directory (refusal #(bfs/atomic-write! (str root "/itself") "x" {:symlink :follow :root root}))))
      (is (= "kept" (slurp outside)))
      (is (= [] (names (str dir "/rootx"))))
      (is (= ::bfs/invalid-options
             (refusal #(bfs/atomic-write! (str root "/in") "x" {:symlink :follow :root (str dir "/nope")}))))
      (testing "a root reached through a link is compared by its real path"
        (fs/create-sym-link (str dir "/alias") "root")
        (is (string? (bfs/atomic-write! (str root "/in") "again" {:symlink :follow :root (str dir "/alias")})))
        (is (= "again" (slurp (str root "/sub/in"))))))))

;; replace-symlink!

(deftest replace-symlink-creates-and-repoints-a-link
  (with-temp-dir [dir]
    (let [link (str dir "/current")]
      (fs/create-dirs (str dir "/v1"))
      (fs/create-dirs (str dir "/v2"))
      (is (= link (bfs/replace-symlink! link "v1")))
      (is (= "v1" (str (fs/read-link link))))
      (bfs/replace-symlink! link (fs/path dir "v2") {:fsync? false})
      (is (= (str dir "/v2") (str (fs/read-link link))))
      (testing "a link to a directory is replaced, the directory left alone"
        (bfs/replace-symlink! link "v1")
        (is (fs/directory? (str dir "/v2")))
        (is (= "v1" (str (fs/read-link link)))))
      (is (= ["current" "v1" "v2"] (names dir))))))

(deftest replace-symlink-creates-and-replaces-dangling-links
  (with-temp-dir [dir]
    (let [link (str dir "/l")]
      (bfs/replace-symlink! link "missing")
      (is (fs/sym-link? link))
      (is (not (fs/exists? link)))
      (bfs/replace-symlink! link "still-missing")
      (is (= "still-missing" (str (fs/read-link link))))
      (is (= ["l"] (names dir))))))

(deftest replace-symlink-refuses-a-directory-and-a-file
  (with-temp-dir [dir]
    (let [d (str dir "/d")
          f (str dir "/f")]
      (fs/create-dir d)
      (spit f "data")
      (is (= ::bfs/is-directory (refusal #(bfs/replace-symlink! d "elsewhere"))))
      (is (= ::bfs/exists (refusal #(bfs/replace-symlink! f "elsewhere"))))
      (is (= "data" (slurp f)))
      (is (fs/directory? d))
      (testing "a directory that appears after the check fails the rename"
        (let [late (str dir "/late")
              real @#'briko.fs/move!]
          (with-redefs-fn {#'briko.fs/move! (fn [from to] (fs/create-dir late) (real from to))}
            (fn [] (is (= ::bfs/failed (refusal #(bfs/replace-symlink! late "x"))))))
          (is (fs/directory? late))))
      (testing ":replace-file? replaces a regular file"
        (bfs/replace-symlink! f "elsewhere" {:replace-file? true})
        (is (fs/sym-link? f)))
      (is (= ["d" "f" "late"] (names dir))))))

(deftest replace-symlink-checks-its-arguments
  (with-temp-dir [dir]
    (is (= ::bfs/no-directory (refusal #(bfs/replace-symlink! (str dir "/no/l") "x"))))
    (is (= ::bfs/invalid-input (refusal #(bfs/replace-symlink! (str dir "/l") ""))))
    (is (= ::bfs/invalid-input (refusal #(bfs/replace-symlink! (str dir "/l") nil))))
    (is (= ::bfs/invalid-input (refusal #(bfs/replace-symlink! (str dir "/l") "a\u0000b"))))
    (is (= ::bfs/invalid-input (refusal #(bfs/replace-symlink! nil "x"))))
    (is (= ::bfs/invalid-options (refusal #(bfs/replace-symlink! (str dir "/l") "x" {:mode "rwx------"}))))
    (is (= ::bfs/invalid-options (refusal #(bfs/replace-symlink! (str dir "/l") "x" {:replace-file? "yes"}))))
    (is (= [] (names dir)))))

(deftest a-failing-rename-removes-the-staged-link
  (with-temp-dir [dir]
    (let [link (str dir "/l")]
      (bfs/replace-symlink! link "old")
      (with-redefs-fn {#'briko.fs/move! (fn [_ _] (throw (IOException. "EIO")))}
        (fn [] (is (= ::bfs/failed (refusal #(bfs/replace-symlink! link "new"))))))
      (is (= "old" (str (fs/read-link link))))
      (is (= ["l"] (names dir))))))

(defn- missing-reads
  "Repoints link between a and b n times with repoint!, while readers
   read the link, and returns how many reads found no link."
  [link n repoint!]
  (let [start (CountDownLatch. 1)
        done (atom false)
        missing (atom 0)
        reader (fn []
                 (future
                   (.await start)
                   (while (not @done)
                     (try
                       (Files/readSymbolicLink (fs/path link))
                       (catch java.nio.file.NoSuchFileException _ (swap! missing inc))))))
        readers [(reader) (reader)]]
    (.countDown start)
    (try
      (loop [i 0]
        (when (and (< i n) (zero? @missing))
          (repoint! (if (even? i) "a" "b"))
          (recur (inc i))))
      (finally (reset! done true)))
    (run! deref readers)
    @missing))

(deftest a-reader-never-finds-the-link-missing
  (with-temp-dir [dir]
    (let [link (str dir "/current")]
      (bfs/replace-symlink! link "a")
      (is (zero? (missing-reads link 3000 #(bfs/replace-symlink! link % {:fsync? false}))))
      (is (= ["current"] (names dir)))
      (testing "negative control: delete and create leaves a window the readers find"
        (is (pos? (missing-reads link 200000 #(do (fs/delete link) (fs/create-sym-link link %)))))))))

(deftest staged-names-are-recognised
  (is (bfs/staged? (str "/x/" (#'briko.fs/temp-name "f"))))
  (is (bfs/staged? (fs/path (#'briko.fs/temp-name "with.dots.edn"))))
  (is (not (bfs/staged? "/x/f")))
  (is (not (bfs/staged? "/x/.f.0b3c.breto")))
  (is (not (bfs/staged? "/x/.f.briko-xyz.tmp")))
  (with-temp-dir [dir]
    (let [seen (atom [])
          real @#'briko.fs/move!]
      (with-redefs-fn {#'briko.fs/move! (fn [from to] (swap! seen conj (bfs/staged? from)) (real from to))}
        (fn []
          (bfs/atomic-write! (str dir "/f") "x")
          (bfs/replace-symlink! (str dir "/l") "f")))
      (is (= [true true] @seen)))))
