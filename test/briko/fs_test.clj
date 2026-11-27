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
           (java.nio.file CopyOption Files StandardCopyOption)
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
