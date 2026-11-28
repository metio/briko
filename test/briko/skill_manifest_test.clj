;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.skill-manifest-test
  "skill-manifest-v1 against manifests tik and kajero signed with their
   own code (skill_manifest_vectors.edn), the acceptance rules both
   projects' tests pin, and a sign and verify round trip through
   ssh-keygen."
  (:require [babashka.fs :as fs]
            [briko.skill-manifest :as sm]
            [briko.ssh-keygen-fixture :as kf]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def vectors
  (edn/read-string (slurp (io/file "test" "briko" "skill_manifest_vectors.edn"))))

(def tik-limits
  "tik's ceilings: the extension's 512 files and 16 MiB per skill."
  {:max-files 512 :max-file-bytes (* 16 1024 1024) :max-skill-bytes (* 16 1024 1024)})

(def kajero-limits
  "kajero's ceilings: Claude Code's 100 KB per file and 400 KB per skill."
  {:max-files 512 :max-file-bytes 100000 :max-skill-bytes 400000})

(def ^:private no-ssh-keygen
  "briko.ssh-sign options naming a program that cannot be started, so a
   verdict reached with them was reached in process."
  {:ssh-keygen "/nonexistent/briko/ssh-keygen"})

(defn- opts [product limits]
  {:namespaces #{(str product "-skill-manifest")} :limits limits :ssh-sign no-ssh-keygen})

(defn- canonical [entry limits] (String. (sm/canonical-bytes entry limits) "UTF-8"))

(defn- sha [c] (str "sha256:" (str/join (repeat 64 c))))

(def ^:private f-md {"uri" "skill://demo/SKILL.md" "digest" (sha "a") "size" 3})
(def ^:private f-a {"uri" "skill://demo/a.md" "digest" (sha "b") "size" 0})
(def ^:private entry {"uri" "skill://demo/SKILL.md" "resources" [f-md f-a]})

;; Golden vectors

(deftest canonical-bytes-equal-what-each-project-produced
  (doseq [product [:tik :kajero]
          {e :entry c :canonical} (get-in vectors [product :entries])]
    (is (= c (canonical e tik-limits)) (str (name product) " " (get e "uri")))
    (is (= c (canonical e kajero-limits)) (str (name product) " " (get e "uri")))))

(deftest both-projects-produced-the-same-bytes-for-the-same-skill
  (let [demo (fn [product] (some #(when (= "skill://demo/SKILL.md" (get-in % [:entry "uri"])) (:canonical %))
                                 (get-in vectors [product :entries])))]
    (is (some? (demo :tik)))
    (is (= (demo :tik) (demo :kajero)))))

(deftest canonical-bytes-match-tiks-recorded-example-and-are-order-free
  (is (= (str "skill-manifest v1\nskill skill://demo/SKILL.md\n"
              "file " (sha "a") " 3 skill://demo/SKILL.md\n"
              "file " (sha "b") " 0 skill://demo/a.md\n")
         (canonical entry tik-limits)))
  (is (= (canonical entry tik-limits) (canonical (assoc entry "resources" [f-a f-md]) tik-limits)))
  (testing "keyword keys read as string keys"
    (is (= (canonical entry tik-limits)
           (canonical {:uri "skill://demo/SKILL.md"
                       :resources [{:uri "skill://demo/SKILL.md" :digest (sha "a") :size 3}
                                   {:uri "skill://demo/a.md" :digest (sha "b") :size 0}]}
                      tik-limits)))))

;; Signatures each project made

(deftest a-manifest-each-project-signed-verifies
  (doseq [product [:tik :kajero]
          {e :entry} (get-in vectors [product :entries])]
    (is (= {:verdict :verified :actor (str (name product) "-fixture") :namespace (str (name product) "-skill-manifest")}
           (sm/verify (get-in vectors [product :allowed-signers]) e (opts (name product) tik-limits)))
        (str (name product) " " (get e "uri")))))

(deftest a-signed-manifest-fails-for-anything-it-was-not-made-for
  (let [{e :entry} (first (get-in vectors [:tik :entries]))
        signers (get-in vectors [:tik :allowed-signers])
        kajero-signers (get-in vectors [:kajero :allowed-signers])
        o (opts "tik" tik-limits)
        meta-path ["_meta" sm/meta-key]]
    (testing "negative control: untouched it verifies"
      (is (= :verified (:verdict (sm/verify signers e o)))))
    (is (= {:verdict :wrong-namespace :namespace "tik-skill-manifest"}
           (sm/verify signers e (opts "kajero" tik-limits)))
        "another product's namespace")
    (is (= :bad-signature (:verdict (sm/verify kajero-signers e o))) "another key")
    (is (= :bad-signature (:verdict (sm/verify signers (assoc-in e (conj meta-path "actor") "mallory") o))) "another actor")
    (is (= :bad-signature (:verdict (sm/verify signers (assoc-in e ["resources" 0 "size"] 1) o))) "another size")
    (is (= :bad-signature (:verdict (sm/verify signers (assoc-in e ["resources" 0 "digest"] (sha "c")) o))) "another digest")
    (is (= :bad-signature (:verdict (sm/verify (str/replace signers "tik-*" "tik-event") e o)))
        "a signers line that does not admit the namespace")
    (is (= {:verdict :missing} (sm/verify signers (update e "_meta" dissoc sm/meta-key) o)))
    (is (= {:verdict :missing} (sm/verify signers (dissoc e "_meta") o)))
    (testing "an entry with keyword keys throughout, as kajero holds it in memory, verifies"
      (let [kw (fn kw [x] (cond (map? x) (update-keys (update-vals x kw) keyword) (sequential? x) (mapv kw x) :else x))]
        (is (= :verified (:verdict (sm/verify signers (kw e) o))))))
    (is (= {:verdict :unsigned :reason "no key"}
           (sm/verify signers (assoc-in e meta-path (sm/unsigned "no key")) o)))
    (doseq [[why m] [["not an object" "signed"]
                     ["unknown format" (assoc (get-in e meta-path) "format" "skill-manifest-v2")]
                     ["signed is a string" (assoc (get-in e meta-path) "signed" "true")]
                     ["no actor" (dissoc (get-in e meta-path) "actor")]
                     ["blank actor" (assoc (get-in e meta-path) "actor" " ")]
                     ["no signature" (dissoc (get-in e meta-path) "signature")]
                     ["a field given twice" (assoc (get-in e meta-path) :actor "tik-fixture")]]]
      (is (= :malformed (:verdict (sm/verify signers (assoc-in e meta-path m) o))) why))
    (is (= :malformed (:verdict (sm/verify signers (assoc-in e ["resources" 0 "uri"] "skill://tik/../tik/SKILL.md") o)))
        "a manifest that cannot be encoded is malformed before its signature is read")
    (is (= :malformed (:verdict (sm/verify signers "not an entry" o))))))

(deftest the-verify-options-are-explicit
  (let [{e :entry} (first (get-in vectors [:tik :entries]))
        signers (get-in vectors [:tik :allowed-signers])]
    (doseq [[why o] [["no namespaces" {:limits tik-limits}]
                     ["empty namespaces" {:namespaces #{} :limits tik-limits}]
                     ["namespaces not a set" {:namespaces ["tik-skill-manifest"] :limits tik-limits}]
                     ["not a skill-manifest namespace" {:namespaces #{"tik-event"} :limits tik-limits}]
                     ["no limits" {:namespaces #{"tik-skill-manifest"}}]
                     ["a limit missing" {:namespaces #{"tik-skill-manifest"} :limits (dissoc tik-limits :max-files)}]
                     ["a limit not positive" {:namespaces #{"tik-skill-manifest"} :limits (assoc tik-limits :max-file-bytes 0)}]]]
      (is (= ::sm/invalid-arguments
             (try (sm/verify signers e o) nil (catch clojure.lang.ExceptionInfo x (:type (ex-data x)))))
          why))
    (is (thrown? clojure.lang.ExceptionInfo (sm/verify nil e (opts "tik" tik-limits))))
    (is (thrown? clojure.lang.ExceptionInfo (sm/canonical-bytes entry nil)))))

;; Acceptance

(deftest entries-both-projects-must-refuse-are-refused
  (doseq [[why e] [["duplicate uri" (assoc entry "resources" [f-md f-md])]
                   ["SKILL.md missing" (assoc entry "resources" [f-a])]
                   ["no files" (assoc entry "resources" [])]
                   ["resources not a list" (assoc entry "resources" "dynamic")]
                   ["a resource not an object" (assoc entry "resources" [f-md "x"])]
                   ["another skill's file" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://x/a.md")])]
                   ["two skills" {"uri" "skill://demo/SKILL.md"
                                  "resources" [f-md (assoc f-md "uri" "skill://other/SKILL.md")]}]
                   ["uppercase digest" (assoc entry "resources" [(update f-md "digest" str/upper-case)])]
                   ["short digest" (assoc entry "resources" [(assoc f-md "digest" "sha256:ab")])]
                   ["another algorithm" (assoc entry "resources" [(assoc f-md "digest" (str "sha512:" (str/join (repeat 64 "a"))))])]
                   ["negative size" (assoc entry "resources" [(assoc f-md "size" -1)])]
                   ["fractional size" (assoc entry "resources" [(assoc f-md "size" 1.5)])]
                   ["size as a string" (assoc entry "resources" [(assoc f-md "size" "3")])]
                   ["uri with a space" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/a b")])]
                   ["uri with a newline" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/a\nfile")])]
                   [".. segment" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/../x.md")])]
                   [". segment" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/./a.md")])]
                   ["empty segment" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo//etc/passwd")])]
                   ["absolute" (assoc entry "resources" [f-md (assoc f-a "uri" "skill:///etc/passwd")])]
                   ["escaped" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/%2e%2e/a.md")])]
                   ["trailing slash" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://demo/a/")])]
                   ["foreign scheme" (assoc entry "resources" [f-md (assoc f-a "uri" "file:///etc/passwd")])]
                   ["skill uri not SKILL.md" (assoc entry "uri" "skill://demo/a.md")]
                   ["skill uri with .." {"uri" "skill://demo/../demo/SKILL.md"
                                         "resources" [(assoc f-md "uri" "skill://demo/../demo/SKILL.md")]}]
                   ["uppercase skill name" {"uri" "skill://Demo/SKILL.md" "resources" [(assoc f-md "uri" "skill://Demo/SKILL.md")]}]
                   ["skill name with a dot" {"uri" "skill://de.mo/SKILL.md" "resources" [(assoc f-md "uri" "skill://de.mo/SKILL.md")]}]
                   ["skill uri missing" (dissoc entry "uri")]
                   ["uri and :uri both" (assoc entry :uri "skill://demo/SKILL.md")]
                   ["not a map" [entry]]]]
    (is (string? (sm/problem e tik-limits)) why)
    (is (contains? sm/malformed-codes (:code (sm/explain e tik-limits))) why)
    (is (= ::sm/malformed (try (sm/canonical-bytes e tik-limits) nil
                               (catch clojure.lang.ExceptionInfo x (:type (ex-data x)))))
        why))
  (testing "negative control: the well-formed entry is accepted"
    (is (nil? (sm/problem entry tik-limits)))
    (is (nil? (sm/explain entry tik-limits)))))

;; Malformed codes

(def ^:private code-limits {:max-files 2 :max-file-bytes 10 :max-skill-bytes 15})

(def ^:private unsigned-marker (sm/unsigned "no key"))

(defn- with-marker [e m] (assoc e "_meta" {sm/meta-key m}))

(def ^:private signed-marker
  {"format" sm/manifest-format "signed" true "namespace" "demo-skill-manifest"
   "actor" "alice" "signature" "-----BEGIN SSH SIGNATURE-----"})

(def ^:private meta-cases
  "A minimal bad manifest for each code about the _meta value, over a
   well-formed entry."
  {:not-an-entry "not an entry"
   :meta-twice (assoc (with-marker entry unsigned-marker) :_meta {sm/meta-key unsigned-marker})
   :value-twice (assoc entry "_meta" {sm/meta-key unsigned-marker (keyword sm/meta-key) unsigned-marker})
   :value-not-an-object (with-marker entry "signed")
   :field-twice (with-marker entry (assoc unsigned-marker :format sm/manifest-format))
   :unknown-format (with-marker entry (assoc unsigned-marker "format" "skill-manifest-v2"))
   :signed-not-boolean (with-marker entry (assoc signed-marker "signed" "true"))
   :no-actor-or-signature (with-marker entry (dissoc signed-marker "actor"))})

(def ^:private entry-cases
  "A minimal bad entry for each code about the entry itself, under
   code-limits."
  (let [files #(assoc entry "resources" (vec %&))]
    {:bad-skill-uri (assoc entry "uri" "skill://Demo/SKILL.md")
     :skill-uri-not-skill-file (assoc entry "uri" "skill://demo/a.md")
     :no-resources (assoc entry "resources" "dynamic")
     :no-files (files)
     :resource-not-an-object (files f-md "x")
     :bad-resource-uri (files f-md (assoc f-a "uri" "skill://demo/../x.md"))
     :foreign-resource (files f-md (assoc f-a "uri" "skill://other/a.md"))
     :bad-digest (files (assoc f-md "digest" "sha256:ab"))
     :bad-size (files (assoc f-md "size" -1))
     :file-too-large (files (assoc f-md "size" 11))
     :duplicate-file (files f-md f-md)
     :skill-file-unlisted (files f-a)
     :too-many-files (files f-md f-a (assoc f-a "uri" "skill://demo/b.md"))
     :skill-too-large (files (assoc f-md "size" 10) (assoc f-a "size" 6))}))

(deftest every-malformed-code-has-a-case-and-every-case-a-code
  (is (= sm/malformed-codes (into (set (keys meta-cases)) (keys entry-cases))))
  (is (every? simple-keyword? sm/malformed-codes)))

(deftest each-meta-code-comes-from-a-minimal-bad-manifest
  (doseq [[code e] meta-cases]
    (testing code
      (let [v (sm/verify "" e {:namespaces #{"demo-skill-manifest"} :limits code-limits :ssh-sign no-ssh-keygen})]
        (is (= :malformed (:verdict v)))
        (is (= code (:code v)))
        (is (and (string? (:reason v)) (not (str/blank? (:reason v)))) "the reason string stays")))))

(deftest each-entry-code-comes-from-a-minimal-bad-entry-in-every-api
  (doseq [[code e] entry-cases]
    (testing code
      (let [{c :code r :reason} (sm/explain e code-limits)
            v (sm/verify "" (with-marker e unsigned-marker)
                         {:namespaces #{"demo-skill-manifest"} :limits code-limits :ssh-sign no-ssh-keygen})
            data (try (sm/canonical-bytes e code-limits) nil (catch clojure.lang.ExceptionInfo x (ex-data x)))]
        (is (= code c) "explain")
        (is (= r (sm/problem e code-limits)) "problem gives explain's reason")
        (is (= {:verdict :malformed :code code :reason r} v) "verify")
        (is (= {:type ::sm/malformed :code code :reason r} data) "canonical-bytes")))))

(deftest negative-controls-for-the-malformed-codes
  (testing "the entry under the cases is well formed under code-limits"
    (is (nil? (sm/explain entry code-limits)))
    (is (some? (sm/canonical-bytes entry code-limits))))
  (testing "the markers under the meta cases pass the malformed checks untouched"
    (is (= {:verdict :unsigned :reason "no key"}
           (sm/verify "" (with-marker entry unsigned-marker)
                      {:namespaces #{"demo-skill-manifest"} :limits code-limits :ssh-sign no-ssh-keygen})))
    (is (not= :malformed
              (:verdict (sm/verify "" (with-marker entry signed-marker)
                                   {:namespaces #{"demo-skill-manifest"} :limits code-limits :ssh-sign no-ssh-keygen}))))
    (is (= {:verdict :missing}
           (sm/verify "" entry {:namespaces #{"demo-skill-manifest"} :limits code-limits :ssh-sign no-ssh-keygen}))))
  (testing "verdicts other than :malformed carry no code"
    (is (not (contains? (sm/verify "" (with-marker entry unsigned-marker)
                                   {:namespaces #{"other-skill-manifest"} :limits code-limits})
                        :code)))))

(deftest the-limits-are-the-callers
  (let [limits {:max-files 2 :max-file-bytes 10 :max-skill-bytes 15}
        at (fn [& sizes] {"uri" "skill://demo/SKILL.md"
                          "resources" (vec (map-indexed (fn [i s] {"uri" (if (zero? i) "skill://demo/SKILL.md" (str "skill://demo/f" i))
                                                                   "digest" (sha "a") "size" s})
                                                        sizes))})]
    (is (nil? (sm/problem (at 10 5) limits)) "at every limit")
    (is (re-find #"exceeds 10 bytes" (sm/problem (at 11) limits)))
    (is (re-find #"exceeds 15 bytes" (sm/problem (at 10 6) limits)))
    (is (re-find #"exceeds 2 files" (sm/problem (at 1 1 1) limits)))
    (is (re-find #"exceeds 10 bytes" (sm/problem (at 1 (bigint 1e30)) limits)) "a size beyond a long")
    (testing "the same entry under kajero's and tik's ceilings"
      (let [big (at 200000)]
        (is (some? (sm/problem big kajero-limits)))
        (is (nil? (sm/problem big tik-limits)))))))

(deftest uris-outside-the-grammar-are-refused
  (is (= ["demo" "SKILL.md"] (sm/parse-uri "skill://demo/SKILL.md")))
  (is (= ["my-skill2" "references/a_b-1.MD"] (sm/parse-uri "skill://my-skill2/references/a_b-1.MD")))
  (doseq [bad ["skill://tik/../tik/SKILL.md" "skill://tik/./SKILL.md" "skill://../SKILL.md"
               "skill://tik//etc/passwd" "skill:///etc/passwd" "skill://tik/"
               "skill://tik" "skill://tik/%2e%2e/SKILL.md" "skill://tik%2FSKILL.md"
               "skill://tik/a%2Fb" "skill://tik/a\\b" "skill://tik/~x" "skill://tik/a b" "skill://tik/SKILL.md/"
               "file:///etc/passwd" "SKILL://tik/SKILL.md" "skill://tik/x\n" "skill://Tik/SKILL.md"
               "skill://-tik/SKILL.md" "skill://tik-/SKILL.md" "skill://ti--k/SKILL.md" "" nil 42]]
    (is (nil? (sm/parse-uri bad)) (pr-str bad))))

;; Files

(deftest files-are-checked-against-their-manifest-line
  (let [b (.getBytes "abc" "UTF-8")
        e {"uri" "skill://demo/SKILL.md" "resources" [{"uri" "skill://demo/SKILL.md" "digest" (sm/digest b) "size" 3}]}]
    (is (= "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (sm/digest b)))
    (is (nil? (sm/check-file e "skill://demo/SKILL.md" b)))
    (is (re-find #"not in the manifest" (sm/check-file e "skill://demo/x.md" b)))
    (is (re-find #"2 bytes" (sm/check-file e "skill://demo/SKILL.md" (.getBytes "ab" "UTF-8"))))
    (is (re-find #"digest" (sm/check-file e "skill://demo/SKILL.md" (.getBytes "xyz" "UTF-8"))))))

(deftest contents-items-decode-to-their-bytes
  (is (= [104 105] (vec (sm/content-bytes {"text" "hi"}))))
  (is (= [0 1 -1 10] (vec (sm/content-bytes {:blob "AAH/Cg=="}))))
  (is (nil? (sm/content-bytes {"uri" "x"}))))

;; Round trip

(deftest a-manifest-signed-here-verifies-and-tampering-fails
  (when (kf/ssh-keygen? "a-manifest-signed-here-verifies-and-tampering-fails")
    (let [dir (fs/create-temp-dir {:prefix "briko-skill-manifest-test-"})]
      (try
        (let [alice (kf/keygen! dir "alice" "ed25519")
              mallory (kf/keygen! dir "mallory" "ed25519")
              signers (str "alice namespaces=\"demo-skill-manifest\" " (:public alice) "\n"
                           "mallory namespaces=\"demo-skill-manifest\" " (:public mallory) "\n")
              marker (sm/sign entry {:key-file (:key alice) :actor "alice" :namespace "demo-skill-manifest" :limits tik-limits})
              signed (assoc entry "_meta" {sm/meta-key marker})
              o {:namespaces #{"demo-skill-manifest"} :limits tik-limits}]
          (is (= #{"format" "signed" "namespace" "actor" "signature"} (set (keys marker))))
          (is (= {:verdict :verified :actor "alice" :namespace "demo-skill-manifest"} (sm/verify signers signed o)))
          (is (= :bad-signature (:verdict (sm/verify signers (assoc-in signed ["_meta" sm/meta-key "actor"] "mallory") o))))
          (is (= :bad-signature (:verdict (sm/verify signers (assoc-in signed ["resources" 1 "size"] 9) o))))
          (testing "ssh-keygen accepts the signature over the canonical bytes"
            (is (kf/keygen-verifies? dir (str "alice namespaces=\"demo-skill-manifest\" " (:public alice)) "alice"
                                     "demo-skill-manifest" (sm/canonical-bytes entry tik-limits) (get marker "signature"))))
          (testing "signing refuses what it cannot encode or name"
            (doseq [o [{:key-file (:key alice) :actor "alice" :namespace "demo-skill-manifest" :limits tik-limits}]
                    [why e] [["two skills" (assoc entry "resources" [f-md (assoc f-a "uri" "skill://x/a.md")])]]]
              (is (thrown? clojure.lang.ExceptionInfo (sm/sign e o)) why))
            (is (thrown? clojure.lang.ExceptionInfo
                         (sm/sign entry {:key-file (:key alice) :actor "alice" :namespace "demo-event" :limits tik-limits})))
            (is (thrown? clojure.lang.ExceptionInfo
                         (sm/sign entry {:key-file (:key alice) :actor "" :namespace "demo-skill-manifest" :limits tik-limits})))))
        (finally (fs/delete-tree dir))))))
