;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.inventory-test
  (:require [babashka.fs :as fs]
            [briko.edn :as bedn]
            [briko.inventory :as inventory]
            [briko.paths :as paths]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (if (or (inventory/refusal? e) (bedn/refusal? e) (paths/refusal? e))
        (:type (ex-data e))
        [:foreign (ex-message e)]))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-inventory-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

(def ^:private vectors
  (edn/read-string (slurp (io/file "test/briko/inventory_vectors.edn"))))

(def ^:private record
  {:host "github.com" :path "metio/aro" :name "aro" :namespace "metio" :default-branch "main"
   :archived false :fork false :topics []})

(defn- text-of
  "The text of an inventory holding records under the source \"s\", built
   by hand so that it can hold what render refuses."
  [& records]
  (str "{\"s\" [" (str/join " " (map pr-str records)) "]}\n"))

;; Compatibility with aro

(deftest the-vectors-cover-the-cases-they-name
  (is (= ["empty" "empty-source" "github-org" "gitlab-and-github-unsorted" "header" "same-repo-in-two-sources"]
         (map :name vectors))))

(deftest render-writes-what-aro-writes
  (doseq [{:keys [name inventory text]} vectors]
    (is (= text (inventory/render inventory)) name)))

(deftest parse-reads-what-aro-reads
  (doseq [{:keys [name text parsed]} vectors]
    (is (= parsed (inventory/parse text)) name)))

(deftest render-of-parse-is-the-identity-on-what-aro-writes
  (doseq [{:keys [name text]} vectors]
    (is (= text (inventory/render (inventory/parse text))) name)))

(deftest parse-reads-the-sources-krei-reads
  ;; krei reads the whole file with clojure.edn, which skips the header's
  ;; comments.
  (doseq [{:keys [name text]} vectors]
    (is (= (edn/read-string text) (:sources (inventory/parse text))) name)))

(deftest render-sorts-sources-by-id-and-records-by-path
  (let [text (inventory/render {:header "" :sources {"b" [(assoc record :path "metio/z") record] "a" [record]}})]
    (is (= ["{\"a\"" " [{:host \"github.com\" :path \"metio/aro\" :name \"aro\" :namespace \"metio\" :default-branch \"main\" :archived false :fork false :topics []}]"
            " \"b\"" " [{:host \"github.com\" :path \"metio/aro\" :name \"aro\" :namespace \"metio\" :default-branch \"main\" :archived false :fork false :topics []}"
            "  {:host \"github.com\" :path \"metio/z\" :name \"aro\" :namespace \"metio\" :default-branch \"main\" :archived false :fork false :topics []}]}"]
           (str/split-lines text)))))

(deftest render-prints-a-large-property-map-sorted
  (let [props (into {} (for [i (range 12)] [(keyword (str "p" i)) (str "v" i)]))
        text (inventory/render {:header "" :sources {"s" [(assoc record :properties props)]}})]
    (is (str/includes? text ":properties {:p0 \"v0\", :p1 \"v1\", :p10 \"v10\""))
    (is (= text (inventory/render (inventory/parse text))))))

(deftest render-ignores-print-bindings
  (let [inv {:header "" :sources {"s" [(assoc record :topics ["a" "b" "c"])]}}
        plain (inventory/render inv)]
    (binding [*print-length* 1 *print-level* 1]
      (is (= plain (inventory/render inv))))))

(deftest a-header-of-comments-and-blank-lines-survives
  (let [text ";; first\n   ;; indented\n\n{}\n"
        parsed (inventory/parse text)]
    (is (= ";; first\n   ;; indented\n" (:header parsed)))
    (is (= {} (:sources parsed)))
    (is (= text (inventory/render parsed)))))

(deftest a-blank-header-renders-nothing
  (is (= "{}\n" (inventory/render {:header "\n  \n" :sources {}})))
  (is (= "{}\n" (inventory/render {:sources {}}))))

(deftest text-without-a-form-has-no-sources
  (is (= {:header "" :sources {}} (inventory/parse "")))
  (is (= {:header ";; only a comment" :sources {}} (inventory/parse ";; only a comment")))
  (is (= {:header "" :sources {}} (inventory/parse "\n\n\n"))))

(deftest a-missing-default-branch-is-nil
  (is (= {"s" [(assoc record :default-branch nil)]}
         (:sources (inventory/parse (text-of (assoc record :default-branch nil)))))))

(deftest one-repository-may-appear-in-two-sources
  (is (= {"a" [record] "b" [record]} (:sources (inventory/parse "{\"a\" [{:host \"github.com\" :path \"metio/aro\" :name \"aro\" :namespace \"metio\" :default-branch \"main\" :archived false :fork false :topics []}] \"b\" [{:host \"github.com\" :path \"metio/aro\" :name \"aro\" :namespace \"metio\" :default-branch \"main\" :archived false :fork false :topics []}]}")))))

(deftest id-joins-host-and-path-with-a-colon
  (is (= "github.com:metio/aro" (inventory/id record)))
  (is (= "gitlab.example.org:group/sub/proj" (inventory/id {:host "gitlab.example.org" :path "group/sub/proj"}))))

;; Refusals: bounds and syntax, through briko.edn

(deftest an-inventory-above-the-byte-limit-is-refused
  (let [text (inventory/render {:header "" :sources {"s" [record]}})
        n (count (.getBytes ^String text "UTF-8"))]
    (is (= :briko.edn/too-large (refusal #(inventory/parse text {:max-bytes (dec n)}))))
    (is (map? (inventory/parse text {:max-bytes n})))
    (testing "the header counts"
      (is (= :briko.edn/too-large (refusal #(inventory/parse (str ";; " (str/join (repeat n "x")) "\n" text)
                                                             {:max-bytes (* 2 n)})))))
    (testing "also without a form"
      (is (= :briko.edn/too-large (refusal #(inventory/parse (str/join (repeat 20 ";")) {:max-bytes 10})))))))

(deftest the-default-limit-holds-thousands-of-repositories
  (let [records (for [i (range 10000)]
                  (assoc record :path (str "org/repository-" i) :name (str "repository-" i)
                         :topics ["kubernetes" "helm"] :properties {:release "maven-central"}))
        text (inventory/render {:header "" :sources {"s" (vec records)}})]
    (is (< (count (.getBytes ^String text "UTF-8")) (:max-bytes inventory/defaults)))
    (is (= 10000 (count (get-in (inventory/parse text) [:sources "s"]))))))

(deftest nesting-deeper-than-an-inventory-is-refused
  (testing "positive control: a property holding a vector is five levels"
    (is (map? (inventory/parse (text-of (assoc record :properties {:features ["a"]}))))))
  (is (= :briko.edn/too-deep (refusal #(inventory/parse (text-of (assoc record :properties {:features [["a"]]}))))))
  (is (= :briko.edn/too-deep (refusal #(inventory/parse (str (str/join (repeat 100000 "[")) (str/join (repeat 100000 "]"))))))))

(deftest tagged-literals-are-refused
  (is (= :briko.edn/tag-refused
         (refusal #(inventory/parse "{\"s\" [{:host \"h\" :path \"p\" :name \"n\" :namespace \"\" :default-branch #inst \"2026-01-01\" :archived false :fork false :topics []}]}"))))
  (is (= :briko.edn/tag-refused (refusal #(inventory/parse "#uuid \"00000000-0000-0000-0000-000000000000\""))))
  (is (= :briko.edn/tag-refused (refusal #(inventory/parse "#my/tag {}")))))

(deftest a-second-form-is-refused
  (is (= :briko.edn/trailing-form (refusal #(inventory/parse "{} {}"))))
  (is (= :briko.edn/malformed (refusal #(inventory/parse "{\"s\" [")))))

;; Refusals: shape

(deftest a-form-that-is-no-map-is-refused
  (doseq [text ["[]" "nil" "\"s\"" "42" ":k"]]
    (is (= ::inventory/invalid-shape (refusal #(inventory/parse text))) text)))

(deftest a-source-id-must-be-a-non-blank-string
  (doseq [text ["{:s []}" "{s []}" "{\"\" []}" "{\" \" []}" "{1 []}" "{nil []}"]]
    (is (= ::inventory/invalid-shape (refusal #(inventory/parse text))) text)))

(deftest records-must-be-a-vector
  (doseq [text ["{\"s\" ()}" "{\"s\" #{}}" "{\"s\" {}}" "{\"s\" nil}"]]
    (is (= ::inventory/invalid-shape (refusal #(inventory/parse text))) text)))

(deftest a-record-must-be-a-map-of-known-keys
  (is (= ::inventory/invalid-shape (refusal #(inventory/parse "{\"s\" [[]]}"))))
  (is (= ::inventory/invalid-shape (refusal #(inventory/parse (text-of (assoc record :stars 3))))))
  (is (= :unknown-keys (:problem (ex-data (try (inventory/parse (text-of (assoc record :stars 3)))
                                               (catch clojure.lang.ExceptionInfo e e)))))))

(deftest a-record-must-hold-every-required-key
  (doseq [k [:host :path :name :namespace :default-branch :archived :fork :topics]]
    (let [e (try (inventory/parse (text-of (dissoc record k))) (catch clojure.lang.ExceptionInfo e e))]
      (is (= ::inventory/invalid-shape (:type (ex-data e))) (str k))
      (is (= [k] (:keys (ex-data e))) (str k)))))

(deftest a-record-value-must-have-its-type
  (doseq [[k v] [[:host ""] [:host nil] [:host :github.com] [:path " "] [:path 1]
                 [:name nil] [:namespace nil] [:default-branch :main] [:default-branch 1]
                 [:archived nil] [:archived "false"] [:fork 0] [:topics nil] [:topics ["a" :b]]
                 [:topics '("a")] [:properties []] [:properties {"release" "x"}]
                 [:properties {:ns/release "x"}] [:properties {:release 1}] [:properties {:release nil}]
                 [:properties {:release ["a" 1]}] [:properties {:release '("a")}]]]
    (let [e (try (inventory/parse (text-of (assoc record k v))) (catch clojure.lang.ExceptionInfo e e))]
      (is (= ::inventory/invalid-shape (:type (ex-data e))) (pr-str [k v]))
      (is (= {:problem :wrong-type :key k :source "s" :index 0}
             (select-keys (ex-data e) [:problem :key :source :index]))
          (pr-str [k v])))))

(deftest a-source-listing-a-repository-twice-is-refused
  (let [e (try (inventory/parse (text-of record (assoc record :name "other")))
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= ::inventory/duplicate-id (:type (ex-data e))))
    (is (= {:source "s" :id "github.com:metio/aro"} (select-keys (ex-data e) [:source :id]))))
  (testing "the same path on two hosts is two repositories"
    (is (map? (inventory/parse (text-of record (assoc record :host "gitlab.com")))))))

(deftest clojure-edn-accepts-what-the-shape-check-refuses
  ;; Negative control: the refusals above come from briko.inventory, not
  ;; from the EDN syntax of the inputs.
  (doseq [[text type] [["[]" ::inventory/invalid-shape]
                       ["{:s []}" ::inventory/invalid-shape]
                       ["{\"s\" ()}" ::inventory/invalid-shape]
                       [(text-of (assoc record :stars 3)) ::inventory/invalid-shape]
                       [(text-of (dissoc record :host)) ::inventory/invalid-shape]
                       [(text-of (assoc record :archived "false")) ::inventory/invalid-shape]
                       [(text-of record record) ::inventory/duplicate-id]]]
    (is (some? (edn/read-string text)) text)
    (is (= type (refusal #(inventory/parse text))) text)))

;; Refusals: render

(deftest render-refuses-what-parse-would-refuse
  (is (= ::inventory/invalid-shape (refusal #(inventory/render {:header "" :sources {"s" [(assoc record :stars 3)]}}))))
  (is (= ::inventory/invalid-shape (refusal #(inventory/render {:header "" :sources {:s [record]}}))))
  (is (= ::inventory/duplicate-id (refusal #(inventory/render {:header "" :sources {"s" [record record]}}))))
  (is (= ::inventory/invalid-input (refusal #(inventory/render {:header "not a comment" :sources {}}))))
  (is (= ::inventory/invalid-input (refusal #(inventory/render {:header ";; ok\n{}" :sources {}}))))
  (is (= ::inventory/invalid-input (refusal #(inventory/render {:header "" :sources {} :extra 1}))))
  (is (= ::inventory/invalid-input (refusal #(inventory/render [])))))

(deftest options-are-checked
  (is (= ::inventory/invalid-options (refusal #(inventory/parse "{}" {:max-byte 1}))))
  (is (= ::inventory/invalid-options (refusal #(inventory/parse "{}" {:max-bytes 0}))))
  (is (= ::inventory/invalid-options (refusal #(inventory/parse "{}" {:missing :empty}))))
  (is (= ::inventory/invalid-options (refusal #(inventory/read-file "x" {:missing :defaults}))))
  (is (= ::inventory/invalid-options (refusal #(inventory/write! "x" {:sources {}} {:fsync? "yes"}))))
  (is (= ::inventory/invalid-options (refusal #(inventory/parse "{}" [])))))

(deftest parse-takes-only-text
  (is (= ::inventory/invalid-input (refusal #(inventory/parse nil))))
  (is (= ::inventory/invalid-input (refusal #(inventory/parse (.getBytes "{}"))))))

;; Files

(deftest read-file-reads-what-write-wrote
  (with-temp-dir [dir]
    (let [f (str dir "/inventory.edn")
          inv {:header ";; header" :sources {"s" [record]}}]
      (is (true? (inventory/write! f inv)))
      (is (= (inventory/render inv) (slurp f)))
      ;; the blank line that ends the header is part of it, as in aro
      (is (= (assoc inv :header ";; header\n") (inventory/read-file f))))))

(deftest read-file-reads-every-vector
  (with-temp-dir [dir]
    (doseq [{:keys [name text parsed]} vectors]
      (let [f (str dir "/" name ".edn")]
        (spit f text)
        (is (= parsed (inventory/read-file f)) name)
        (is (= parsed (inventory/read-file (fs/path f))) name)
        (is (= parsed (inventory/read-file (io/file f))) name)))))

(deftest a-missing-file-is-refused-or-empty
  (with-temp-dir [dir]
    (let [f (str dir "/none.edn")
          e (try (inventory/read-file f) (catch clojure.lang.ExceptionInfo e e))]
      (is (= ::inventory/missing (:type (ex-data e))))
      (is (= f (:file (ex-data e))))
      (is (= {:header "" :sources {}} (inventory/read-file f {:missing :empty}))))))

(deftest a-file-that-is-not-regular-is-refused
  (with-temp-dir [dir]
    (is (= ::inventory/not-a-file (refusal #(inventory/read-file dir {:missing :empty}))))
    (let [link (str dir "/dangling.edn")]
      (fs/create-sym-link link (str dir "/nowhere.edn"))
      (is (= ::inventory/not-a-file (refusal #(inventory/read-file link {:missing :empty})))))
    (let [target (str dir "/real.edn")
          link (str dir "/link.edn")]
      (spit target "{}\n")
      (fs/create-sym-link link target)
      (is (= {:header "" :sources {}} (inventory/read-file link))))))

(deftest read-file-applies-the-byte-limit-before-reading-on
  (with-temp-dir [dir]
    (let [f (str dir "/big.edn")
          text (inventory/render {:header "" :sources {"s" [record]}})
          n (count (.getBytes ^String text "UTF-8"))]
      (spit f text)
      (let [e (try (inventory/read-file f {:max-bytes (dec n)}) (catch clojure.lang.ExceptionInfo e e))]
        (is (= ::inventory/too-large (:type (ex-data e))))
        (is (= f (:file (ex-data e)))))
      (is (map? (inventory/read-file f {:max-bytes n}))))))

(deftest read-file-refuses-invalid-utf-8-and-names-the-file
  (with-temp-dir [dir]
    (let [f (str dir "/bad.edn")]
      (with-open [out (io/output-stream f)]
        (.write out (byte-array [0x7B 0x22 (unchecked-byte 0xC3) 0x22 0x20 0x5B 0x5D 0x7D])))
      (let [e (try (inventory/read-file f) (catch clojure.lang.ExceptionInfo e e))]
        (is (= :briko.edn/invalid-utf8 (:type (ex-data e))))
        (is (= f (:file (ex-data e))))))
    (let [f (str dir "/shape.edn")]
      (spit f "[]")
      (is (= f (:file (ex-data (try (inventory/read-file f) (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest write-creates-directories-and-reports-changes
  (with-temp-dir [dir]
    (let [f (str dir "/state/aro/inventory.edn")
          inv {:header "" :sources {"s" [record]}}]
      (is (true? (inventory/write! f inv)))
      (is (false? (inventory/write! f inv)))
      (is (false? (inventory/write! f (inventory/read-file f))))
      (is (true? (inventory/write! f (assoc-in inv [:sources "t"] [record])))))))

(deftest write-keeps-the-header-it-read
  (with-temp-dir [dir]
    (let [f (str dir "/inventory.edn")]
      (spit f ";; a licence header\n\n{}\n")
      (is (true? (inventory/write! f (assoc (inventory/read-file f) :sources {"metio" [record]}))))
      (is (str/starts-with? (slurp f) ";; a licence header\n\n{\"metio\"")))))

(deftest write-replaces-the-file-in-one-rename
  (with-temp-dir [dir]
    (let [f (str dir "/inventory.edn")
          inode #(get (fs/read-attributes f "unix:ino") :ino)]
      (spit f "{}\n")
      (let [before (inode)]
        (is (true? (inventory/write! f {:header "" :sources {"s" [record]}} {:fsync? false})))
        (is (not= before (inode)))
        (is (= ["inventory.edn"] (map (comp str fs/file-name) (fs/list-dir dir))))))))

(deftest a-refused-write-leaves-the-file-alone
  (with-temp-dir [dir]
    (let [f (str dir "/inventory.edn")
          inv {:header "" :sources {"s" [record]}}
          n (count (.getBytes ^String (inventory/render inv) "UTF-8"))]
      (spit f "{}\n")
      (is (= ::inventory/too-large (refusal #(inventory/write! f inv {:max-bytes (dec n)}))))
      (is (= ::inventory/invalid-shape (refusal #(inventory/write! f {:header "" :sources {"s" [(dissoc record :host)]}}))))
      (is (= "{}\n" (slurp f)))
      (is (= ["inventory.edn"] (map (comp str fs/file-name) (fs/list-dir dir))))
      (is (true? (inventory/write! f inv {:max-bytes n}))))))

(deftest write-refuses-a-blank-path
  (is (= ::inventory/invalid-input (refusal #(inventory/write! "" {:sources {}}))))
  (is (= ::inventory/invalid-input (refusal #(inventory/read-file nil)))))

;; Default path

(defn- env [m]
  {:getenv (fn [n] (get m n)) :getprop (fn [n] (get {"user.home" "/home/prop"} n))})

(deftest the-default-path-is-in-the-state-home
  (is (= "/state/aro/inventory.edn" (inventory/default-path (env {"XDG_STATE_HOME" "/state" "HOME" "/home/me"}))))
  (is (= "/home/me/.local/state/aro/inventory.edn" (inventory/default-path (env {"HOME" "/home/me"}))))
  (testing "an empty, blank or relative XDG_STATE_HOME counts as unset"
    (doseq [v ["" "  " "state" "./state"]]
      (is (= "/home/me/.local/state/aro/inventory.edn"
             (inventory/default-path (env {"XDG_STATE_HOME" v "HOME" "/home/me"})))
          v)))
  (testing "without an absolute HOME, the user.home property"
    (is (= "/home/prop/.local/state/aro/inventory.edn" (inventory/default-path (env {}))))
    (is (= "/home/prop/.local/state/aro/inventory.edn" (inventory/default-path (env {"HOME" "relative"})))))
  (testing "without any home"
    (is (= :briko.paths/no-home
           (refusal #(inventory/default-path {:getenv (constantly nil) :getprop (constantly nil)})))))
  (testing "the process environment by default"
    (is (str/ends-with? (inventory/default-path) "/aro/inventory.edn"))))
