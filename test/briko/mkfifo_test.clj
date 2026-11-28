;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.mkfifo-test
  "How briko.process creates its named pipes: with the mkfifo program, or
   on a JVM with native access through briko.mkfifo, which is loaded only
   at run time because babashka has no foreign function API."
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private jvm?
  (nil? (System/getProperty "babashka.version")))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (fs/create-temp-dir {:prefix "briko-mkfifo-test-"})]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- kind-and-mode
  "What stat says the file at path is, and its permission bits, such as
   \"fifo 600\"."
  [path]
  (str/trim (:out (process/run ["stat" "-c" "%F %a" (str path)]))))

(defn- private-fn
  "The function of the var named sym, loaded at run time."
  [sym]
  @(requiring-resolve sym))

(deftest the-mkfifo-program-makes-owner-only-pipes
  (with-temp-dir [dir]
    (let [paths [(str (fs/path dir "out")) (str (fs/path dir "err"))]]
      (is (nil? (#'briko.process/run-mkfifo! paths (+ (System/nanoTime) 60000000000))))
      (doseq [p paths]
        (is (= "fifo 600" (kind-and-mode p)) p)))))

(deftest a-missing-mkfifo-program-answers-127
  (with-redefs-fn {#'briko.process/native-mkfifo (delay nil)
                   #'briko.process/mkfifo-program "/nonexistent/briko-no-mkfifo"}
    #(let [r (process/run ["echo" "x"])]
       (is (= 127 (:exit r)))
       (is (false? (:launched? r)))
       (is (str/includes? (:err r) "mkfifo could not run") (:err r))))
  (testing "negative control: the same run with the program on the PATH"
    (with-redefs-fn {#'briko.process/native-mkfifo (delay nil)}
      #(is (= {:exit 0 :out "x\n"} (select-keys (process/run ["echo" "x"]) [:exit :out]))))))

(deftest a-pipe-that-cannot-be-made-in-process-answers-127
  (with-redefs-fn {#'briko.process/native-mkfifo (delay (fn [_] "mkfifo(3) failed with errno 13"))}
    #(let [r (process/run ["echo" "x"])]
       (is (= 127 (:exit r)))
       (is (false? (:launched? r)))
       (is (str/includes? (:err r) "errno 13") (:err r)))))

(deftest the-runtime-decides-how-pipes-are-made
  (if-not jvm?
    (is (nil? @@#'briko.process/native-mkfifo) "babashka runs the mkfifo program")
    (is (some? @@#'briko.process/native-mkfifo)
        "the test alias enables native access, so the JVM suite exercises mkfifo(3)"))
  (when jvm?
    (testing "a run needs no mkfifo program"
      (with-redefs-fn {#'briko.process/run-mkfifo! (fn [_ _] (throw (IllegalStateException. "the program was used")))}
        #(is (= {:exit 0 :out "x\n" :err "e\n"}
                (select-keys (process/run ["sh" "-c" "echo x; echo e >&2"]) [:exit :out :err])))))))

(deftest mkfifo-3-makes-owner-only-pipes-and-names-errno
  (when jvm?
    (let [make! ((private-fn 'briko.mkfifo/maker))]
      (with-temp-dir [dir]
        (let [p (str (fs/path dir "out"))]
          (is (nil? (make! p)))
          (is (= "fifo 600" (kind-and-mode p)))
          (is (str/includes? (str (make! p)) "errno 17") "an existing path is EEXIST")
          (is (str/includes? (str (make! (str (fs/path dir "missing" "out")))) "errno 2")
              "a missing directory is ENOENT"))))))

(deftest owner-only-sets-the-mode-the-umask-changed
  (when jvm?
    (let [owner-only! (private-fn 'briko.mkfifo/owner-only!)]
      (with-temp-dir [dir]
        (let [fifo (str (fs/path dir "wide"))
              file (str (fs/path dir "file"))]
          (is (zero? (:exit (process/run ["mkfifo" "-m" "644" fifo]))))
          (is (= "fifo 644" (kind-and-mode fifo)) "negative control: the pipe starts readable by others")
          (is (nil? (owner-only! fifo)))
          (is (= "fifo 600" (kind-and-mode fifo)))
          (spit file "")
          (is (str/includes? (str (owner-only! file)) "with mode 100600")
              "a regular file is not taken for a pipe"))))))

(deftest native-access-decides-and-nothing-warns
  (when jvm?
    (let [java (str (fs/path (System/getProperty "java.home") "bin" "java"))
          cp (System/getProperty "java.class.path")
          probe (str "(require 'briko.process)"
                     "(prn (:exit (briko.process/run [\"echo\" \"x\"])))"
                     "(prn (some? @@#'briko.process/native-mkfifo))"
                     "(shutdown-agents)")
          run-java (fn [flags]
                     (process/run (-> [java] (into flags) (into ["-cp" cp "clojure.main" "-e" probe]))
                                  {:timeout-ms 120000}))]
      (testing "without native access the program is used and the JVM prints no warning"
        (let [r (run-java [])]
          (is (= "0\nfalse\n" (:out r)) (:err r))
          (is (not (str/includes? (:err r) "WARNING")) (:err r))))
      (testing "with native access the pipes are made in process"
        (let [r (run-java ["--enable-native-access=ALL-UNNAMED"])]
          (is (= "0\ntrue\n" (:out r)) (:err r))
          (is (not (str/includes? (:err r) "WARNING")) (:err r)))))))
