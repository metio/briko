;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.secret-test
  "Every secret in these tests is a dummy value."
  (:require [babashka.fs :as fs]
            [briko.process :as process]
            [briko.secret :as secret]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (secret/refusal? e) (:type (ex-data e)) [:foreign (ex-message e)]))))

(defn- refusal-text
  "The message and data of the refusal (f) throws, as one string."
  [f]
  (try
    (f)
    "no refusal"
    (catch clojure.lang.ExceptionInfo e
      (str (ex-message e) " " (pr-str (ex-data e))))))

(defn- env
  "A :getenv option reading m."
  [m]
  {:getenv (fn [n] (get m n))})

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (fs/create-temp-dir {:prefix "briko-secret-"})]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(defn- write!
  [dir n content]
  (let [p (fs/path dir n)]
    (if (bytes? content)
      (fs/write-bytes p content)
      (spit (str p) content))
    (str p)))

(def ^:private dummy "dummy-secret-0x5eed")

;; Plain values

(deftest strings-and-nil-pass-through
  (is (= "literal" (secret/resolve1 "token" "literal")))
  (is (= "" (secret/resolve1 "token" "")))
  (is (nil? (secret/resolve1 "token" nil))))

(deftest spec?-wants-one-source-key
  (is (secret/spec? {:env "X"}))
  (is (secret/spec? {:file "/x"}))
  (is (secret/spec? {:credential "x"}))
  (is (secret/spec? {:command ["x"]}))
  (is (not (secret/spec? {})))
  (is (not (secret/spec? {:envv "X"})))
  (is (not (secret/spec? {:env "X" :default "y"})))
  (is (not (secret/spec? {:type :command :command ["x"]})))
  (is (not (secret/spec? "x")))
  (is (not (secret/spec? [:env "X"]))))

;; :env

(deftest env-specs-read-the-variable
  (is (= dummy (secret/resolve1 "token" {:env "TOKEN"} (env {"TOKEN" dummy}))))
  (testing "the value is used as it is set"
    (is (= " padded " (secret/resolve1 "token" {:env "TOKEN"} (env {"TOKEN" " padded "}))))))

(deftest env-specs-read-the-real-environment-by-default
  (is (= (System/getenv "PATH") (secret/resolve1 "path" {:env "PATH"}))))

(deftest env-specs-refuse-unset-empty-and-blank-variables
  (is (= ::secret/unset (refusal #(secret/resolve1 "token" {:env "TOKEN"} (env {})))))
  (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:env "TOKEN"} (env {"TOKEN" ""})))))
  (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:env "TOKEN"} (env {"TOKEN" " \t\n"})))))
  (let [data (try (secret/resolve1 "token" {:env "TOKEN"} (env {"TOKEN" ""}))
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= {:type ::secret/empty :secret "token" :env "TOKEN"} data))))

(deftest env-specs-refuse-an-invalid-name
  (doseq [n ["" "  " "A=B" "a\u0000b" 1 nil :token]]
    (is (= ::secret/invalid-spec (refusal #(secret/resolve1 "token" {:env n} (env {})))) (pr-str n))))

;; :file

(deftest file-specs-read-the-first-line-trimmed
  (with-temp-dir [dir]
    (is (= dummy (secret/resolve1 "token" {:file (write! dir "a" (str dummy "\n"))})))
    (is (= dummy (secret/resolve1 "token" {:file (write! dir "b" (str "  " dummy "  \r\nlogin: x\n"))})))
    (is (= dummy (secret/resolve1 "token" {:file (write! dir "c" dummy)})))))

(deftest file-specs-follow-a-link-to-a-regular-file
  (with-temp-dir [dir]
    (let [target (write! dir "target" dummy)
          link (fs/path dir "link")]
      (fs/create-sym-link link target)
      (is (= dummy (secret/resolve1 "token" {:file (str link)}))))))

(deftest file-specs-refuse-empty-and-blank-files
  (with-temp-dir [dir]
    (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:file (write! dir "empty" "")}))))
    (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:file (write! dir "blank" " \n\t\n")}))))
    (testing "a blank first line is refused rather than skipped to a field"
      (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:file (write! dir "field" "\nlogin: x\n")})))))))

(deftest file-specs-refuse-what-is-not-a-readable-regular-file
  (with-temp-dir [dir]
    (is (= ::secret/missing (refusal #(secret/resolve1 "token" {:file (str (fs/path dir "missing"))}))))
    (fs/create-sym-link (fs/path dir "dangling") (fs/path dir "nowhere"))
    (is (= ::secret/missing (refusal #(secret/resolve1 "token" {:file (str (fs/path dir "dangling"))}))))
    (is (= ::secret/not-a-file (refusal #(secret/resolve1 "token" {:file (str dir)}))))
    (let [fifo (str (fs/path dir "fifo"))]
      (is (zero? (:exit (process/run ["mkfifo" fifo]))))
      (is (= ::secret/not-a-file (refusal #(secret/resolve1 "token" {:file fifo})))))
    (let [locked (write! dir "locked" dummy)]
      (fs/set-posix-file-permissions locked "---------")
      ;; root reads a file whatever its mode, so only check where the
      ;; mode is enforced.
      (when-not (fs/readable? locked)
        (is (= ::secret/unreadable (refusal #(secret/resolve1 "token" {:file locked}))))))))

(deftest file-specs-refuse-large-and-binary-files
  (with-temp-dir [dir]
    (let [f (write! dir "big" (str/join (repeat 101 "x")))]
      (is (= ::secret/too-large (refusal #(secret/resolve1 "token" {:file f} {:max-bytes 100}))))
      (is (= (str/join (repeat 101 "x")) (secret/resolve1 "token" {:file f} {:max-bytes 101}))))
    (let [f (write! dir "default-limit" (str/join (repeat (inc (:max-bytes secret/defaults)) "x")))]
      (is (= ::secret/too-large (refusal #(secret/resolve1 "token" {:file f})))))
    (let [f (write! dir "binary" (byte-array (map unchecked-byte [0x61 0xff 0xfe 0x0a])))]
      (is (= ::secret/not-text (refusal #(secret/resolve1 "token" {:file f})))))))

(deftest file-refusals-name-the-file-but-not-its-content
  (with-temp-dir [dir]
    (let [f (write! dir "big" (str dummy (str/join (repeat 200 "x"))))
          text (refusal-text #(secret/resolve1 "token" {:file f} {:max-bytes 100}))]
      (is (str/includes? text f))
      (is (not (str/includes? text dummy))))))

;; :credential

(deftest credential-specs-read-from-the-credentials-directory
  (with-temp-dir [dir]
    (write! dir "api-token" (str dummy "\n"))
    (is (= dummy (secret/resolve1 "token" {:credential "api-token"}
                                  (env {"CREDENTIALS_DIRECTORY" (str dir)}))))))

(deftest credential-specs-refuse-without-a-credentials-directory
  (doseq [dir [nil "" "  " "relative/dir"]]
    (is (= ::secret/no-credentials-directory
           (refusal #(secret/resolve1 "token" {:credential "x"} (env {"CREDENTIALS_DIRECTORY" dir}))))
        (pr-str dir))))

(deftest credential-specs-refuse-names-that-leave-the-directory
  (with-temp-dir [dir]
    (let [inner (fs/create-dirs (fs/path dir "creds"))]
      (write! dir "outside" dummy)
      (write! inner "inside" dummy)
      (doseq [n ["../outside" ".." "." "a/b" "/etc/passwd" "a\\b" "a..b" "" " " "a\u0000b" 1 nil :x]]
        (is (= ::secret/invalid-name
               (refusal #(secret/resolve1 "token" {:credential n} (env {"CREDENTIALS_DIRECTORY" (str inner)}))))
            (pr-str n)))
      (is (= dummy (secret/resolve1 "token" {:credential "inside"} (env {"CREDENTIALS_DIRECTORY" (str inner)})))))))

(deftest credential-specs-refuse-missing-and-empty-credentials
  (with-temp-dir [dir]
    (write! dir "empty" "")
    (let [opts (env {"CREDENTIALS_DIRECTORY" (str dir)})]
      (is (= ::secret/missing (refusal #(secret/resolve1 "token" {:credential "absent"} opts))))
      (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:credential "empty"} opts)))))))

;; :command

(deftest command-specs-yield-the-first-line-of-stdout
  (is (= dummy (secret/resolve1 "token" {:command ["printf" "%s\\nlogin: x\\n" dummy]})))
  (is (= dummy (secret/resolve1 "token" {:command (str "printf '  %s  \\n' " dummy)}))))

(deftest command-vectors-run-without-a-shell
  (is (= "$HOME;" (secret/resolve1 "token" {:command ["printf" "%s" "$HOME;"]}))))

(deftest command-specs-refuse-blank-output
  (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:command ["true"]}))))
  (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:command ["printf" "  \\n\\n"]}))))
  (is (= ::secret/empty (refusal #(secret/resolve1 "token" {:command ["printf" "\\nlogin: x\\n"]})))))

(deftest command-specs-refuse-a-failing-command-without-its-output
  (let [spec {:command ["sh" "-c" (str "printf %s " dummy "; exit 3")]}]
    (is (= ::secret/command-failed (refusal #(secret/resolve1 "token" spec))))
    (let [data (try (secret/resolve1 "token" spec) (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:type ::secret/command-failed :secret "token" :program "sh" :exit 3} data)))
    (is (not (str/includes? (refusal-text #(secret/resolve1 "token" spec)) dummy)))))

(deftest command-specs-refuse-a-command-past-its-deadline
  (let [start (System/nanoTime)]
    (is (= ::secret/command-timed-out
           (refusal #(secret/resolve1 "token" {:command ["sleep" "30"]} {:timeout-ms 300}))))
    (is (< (quot (- (System/nanoTime) start) 1000000) 5000))))

(deftest command-specs-refuse-a-command-that-cannot-run
  (is (= ::secret/command-not-run (refusal #(secret/resolve1 "token" {:command ["briko-no-such-program-0f3a"]})))))

(deftest command-specs-refuse-large-output
  (is (= ::secret/too-large (refusal #(secret/resolve1 "token" {:command ["head" "-c" "101" "/dev/zero"]}
                                                       {:max-bytes 100}))))
  (is (= ::secret/too-large (refusal #(secret/resolve1 "token" {:command ["head" "-c" "1000000" "/dev/zero"]})))))

(deftest command-specs-refuse-an-invalid-command
  (doseq [c [[] "" "  " [""] ["printf" 1] [:printf] 1 nil {:argv ["x"]}]]
    (is (= ::secret/invalid-spec (refusal #(secret/resolve1 "token" {:command c}))) (pr-str c))))

;; Shapes that are not specs

(deftest values-that-are-neither-strings-nor-specs-are-refused
  (doseq [x [{:envv "X"} {:env "X" :default "y"} {} 1234 :token ["x"] {:password dummy}]]
    (is (= ::secret/invalid-spec (refusal #(secret/resolve1 "token" x))) (pr-str x)))
  (testing "a refusal describes the value without printing it"
    (is (not (str/includes? (refusal-text #(secret/resolve1 "token" {:password dummy})) dummy)))
    (is (not (str/includes? (refusal-text #(secret/resolve1 "token" 987654321)) "987654321")))))

(deftest unknown-and-invalid-options-are-refused
  (doseq [opts [[] {:timeout 1} {:timeout-ms 0} {:timeout-ms "1"} {:max-bytes 0}
                {:max-bytes Integer/MAX_VALUE} {:getenv "x"}]]
    (is (= ::secret/invalid-options (refusal #(secret/resolve1 "token" "x" opts))) (pr-str opts))))

;; resolve-secrets

(defrecord Settings [url token])

(deftest resolve-secrets-walks-any-data
  (let [opts (env {"TOKEN" dummy "URL" "https://example.org/hook"})
        config {:url {:env "URL"}
                :headers {"Authorization" {:env "TOKEN"}}
                :list [{:env "TOKEN"} "plain" 1]
                :seq (list {:env "TOKEN"})
                :set #{{:env "TOKEN"}}
                :sink {:type :command :command ["echo" "not a secret"]}
                :n 42}]
    (is (= {:url "https://example.org/hook"
            :headers {"Authorization" dummy}
            :list [dummy "plain" 1]
            :seq (list dummy)
            :set #{dummy}
            :sink {:type :command :command ["echo" "not a secret"]}
            :n 42}
           (secret/resolve-secrets nil config opts)))))

(deftest resolve-secrets-keeps-the-kind-of-every-map
  (let [opts (env {"TOKEN" dummy})
        rec (secret/resolve-secrets nil (->Settings "u" {:env "TOKEN"}) opts)
        sorted (secret/resolve-secrets nil (sorted-map :b {:env "TOKEN"} :a 1) opts)
        meta-map (secret/resolve-secrets nil (with-meta {:a {:env "TOKEN"}} {:m 1}) opts)]
    (is (instance? Settings rec))
    (is (= dummy (:token rec)))
    (is (sorted? sorted))
    (is (= [:a :b] (keys sorted)))
    (is (= {:m 1} (meta meta-map)))))

(deftest resolve-secrets-names-the-field-in-a-refusal
  (let [data (try (secret/resolve-secrets nil {:headers {"Authorization" {:env "TOKEN"}}} (env {}))
                  (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= "Authorization" (:secret data)))
    (is (= ::secret/unset (:type data)))))

(deftest resolve-secrets-has-the-arities-of-tik
  (is (= {:a "x"} (secret/resolve-secrets {:a "x"})))
  (is (= ::secret/invalid-spec (refusal #(secret/resolve-secrets "top" {:env 1}))))
  (is (= ::secret/invalid-options (refusal #(secret/resolve-secrets nil {} {:bogus 1})))))

;; first-line and field-in

(deftest first-line-takes-line-one
  (is (= "pw" (secret/first-line "  pw  \nlogin: x")))
  (is (= "pw" (secret/first-line "pw\r\nx")))
  (is (nil? (secret/first-line "")))
  (is (nil? (secret/first-line nil)))
  (is (nil? (secret/first-line "\nsecond"))))

(deftest field-in-finds-named-fields
  (let [s "pw\nlogin: alice\nurl: https://a.example:8443/x\nempty:   \n"]
    (is (= "alice" (secret/field-in s "login")))
    (is (= "https://a.example:8443/x" (secret/field-in s "url")))
    (is (nil? (secret/field-in s "empty")))
    (is (nil? (secret/field-in s "missing")))
    (is (nil? (secret/field-in nil "login")))))

;; materialize-file

(defn- mode
  [path]
  (fs/posix->str (fs/posix-file-permissions path)))

(deftest materialize-file-uses-a-file-where-it-lies
  (with-temp-dir [dir]
    (let [f (write! dir "key" "-----BEGIN KEY-----\ndummy\n-----END KEY-----\n")]
      (is (= f (secret/materialize-file "key" f dir "unused")))
      (is (= f (secret/materialize-file "key" {:file f} dir "unused")))
      (testing "the whole content counts, not the first line"
        (let [g (write! dir "leading-blank" "\n-----BEGIN KEY-----\n")]
          (is (= g (secret/materialize-file "key" g dir "unused"))))))))

(deftest materialize-file-refuses-missing-and-blank-files
  (with-temp-dir [dir]
    (is (= ::secret/missing (refusal #(secret/materialize-file "key" (str (fs/path dir "x")) dir "n"))))
    (is (= ::secret/empty (refusal #(secret/materialize-file "key" (write! dir "e" "") dir "n"))))
    (is (= ::secret/empty (refusal #(secret/materialize-file "key" (write! dir "b" " \n ") dir "n"))))))

(deftest materialize-file-uses-a-credential-file
  (with-temp-dir [dir]
    (let [f (write! dir "key" "dummy key\n")]
      (is (= f (secret/materialize-file "key" {:credential "key"} dir "unused"
                                        (env {"CREDENTIALS_DIRECTORY" (str dir)}))))
      (is (= ::secret/invalid-name
             (refusal #(secret/materialize-file "key" {:credential "../key"} dir "unused"
                                                (env {"CREDENTIALS_DIRECTORY" (str dir)}))))))))

(deftest materialize-file-writes-command-output-owner-only
  (with-temp-dir [dir]
    (let [path (secret/materialize-file "key" {:command ["printf" "line1\\nline2"]} dir "key")]
      (is (= (str (fs/path dir "key")) path))
      (is (= "line1\nline2\n" (slurp path)))
      (is (= "rw-------" (mode path))))
    (let [path (secret/materialize-file "key" {:command ["printf" "whole\\n"]} dir "key2")]
      (is (= "whole\n" (slurp path))))))

(deftest materialize-file-never-replaces-or-follows
  (with-temp-dir [dir]
    (write! dir "taken" "existing")
    (fs/create-sym-link (fs/path dir "link") (fs/path dir "elsewhere"))
    (is (= ::secret/exists (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} dir "taken"))))
    (is (= "existing" (slurp (str (fs/path dir "taken")))))
    (is (= ::secret/exists (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} dir "link"))))
    (is (not (fs/exists? (fs/path dir "elsewhere"))))))

(deftest materialize-file-wants-a-private-directory
  (with-temp-dir [dir]
    (let [open-dir (fs/create-dirs (fs/path dir "open"))]
      (fs/set-posix-file-permissions open-dir "rwxr-x---")
      (is (= ::secret/insecure-directory
             (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} open-dir "n"))))
      (is (not (fs/exists? (fs/path open-dir "n")))))
    (is (= ::secret/invalid-directory
           (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} (fs/path dir "missing") "n"))))
    (is (= ::secret/invalid-directory
           (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} nil "n"))))))

(deftest materialize-file-wants-a-bare-file-name
  (with-temp-dir [dir]
    (doseq [n ["../x" "a/b" ".." "." "" nil]]
      (is (= ::secret/invalid-name (refusal #(secret/materialize-file "key" {:command ["echo" "x"]} dir n)))
          (pr-str n)))))

(deftest materialize-file-writes-nothing-for-a-failing-or-blank-command
  (with-temp-dir [dir]
    (is (= ::secret/command-failed (refusal #(secret/materialize-file "key" {:command ["false"]} dir "a"))))
    (is (= ::secret/empty (refusal #(secret/materialize-file "key" {:command ["printf" " \\n"]} dir "b"))))
    (is (= ::secret/command-timed-out
           (refusal #(secret/materialize-file "key" {:command ["sleep" "30"]} dir "c" {:timeout-ms 300}))))
    (is (empty? (fs/list-dir dir)))))

(deftest materialize-file-refuses-env-and-other-shapes
  (with-temp-dir [dir]
    (is (= ::secret/refused-source (refusal #(secret/materialize-file "key" {:env "KEY"} dir "n"))))
    (is (= ::secret/invalid-spec (refusal #(secret/materialize-file "key" {:file "x" :mode 1} dir "n"))))
    (is (= ::secret/invalid-spec (refusal #(secret/materialize-file "key" 1 dir "n"))))))
