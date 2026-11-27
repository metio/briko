;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.paths-test
  (:require [babashka.fs :as fs]
            [briko.paths :as paths]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn- refusal
  "The :type of the refusal (f) throws, or :no-refusal when it returns."
  [f]
  (try
    (f)
    :no-refusal
    (catch clojure.lang.ExceptionInfo e
      (:type (ex-data e)))))

(defn- env
  "Options reading the environment m and the properties props."
  ([m] (env m {"user.home" "/passwd/home"}))
  ([m props] {:getenv (fn [n] (get m n)) :getprop (fn [n] (get props n))}))

(defn- holds?
  [num-tests property]
  (let [result (tc/quick-check num-tests property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :fail :shrunk])))))

(defmacro ^:private with-temp-dir
  [[sym] & body]
  `(let [~sym (str (fs/create-temp-dir {:prefix "briko-paths-"}))]
     (try (let [res# (do ~@body)] res#) (finally (fs/delete-tree ~sym)))))

;; Home

(deftest home-prefers-an-absolute-HOME
  (is (= "/h" (paths/home (env {"HOME" "/h"}))))
  (is (= "/h" (paths/home (env {"HOME" "/h/"})))))

(deftest home-falls-back-to-user-home-for-unset-blank-or-relative-HOME
  (doseq [value [nil "" "   " "relative/home" "~" "./x"]]
    (testing (pr-str value)
      (is (= "/passwd/home" (paths/home (env {"HOME" value})))))))

(deftest home-refuses-when-neither-is-absolute
  (is (= ::paths/no-home (refusal #(paths/home (env {} {})))))
  (is (= ::paths/no-home (refusal #(paths/home (env {"HOME" ""} {"user.home" "?"})))))
  (is (= ::paths/no-home (refusal #(paths/config-home (env {"HOME" "x"} {"user.home" ""}))))))

(deftest home-of-the-real-environment-is-absolute
  (is (fs/absolute? (paths/home))))

;; Base directories

(def ^:private homes
  [[paths/config-home "XDG_CONFIG_HOME" "/h/.config"]
   [paths/data-home "XDG_DATA_HOME" "/h/.local/share"]
   [paths/state-home "XDG_STATE_HOME" "/h/.local/state"]
   [paths/cache-home "XDG_CACHE_HOME" "/h/.cache"]])

(deftest each-base-directory-honours-an-absolute-value
  (doseq [[f variable _] homes]
    (testing variable
      (is (= "/x/y" (f (env {"HOME" "/h" variable "/x/y"}))))
      (is (= "/x/y" (f (env {"HOME" "/h" variable "/x//y/"})))))))

(deftest each-base-directory-ignores-unset-empty-blank-and-relative-values
  (doseq [[f variable fallback] homes
          value [nil "" " " "\t" "relative" "./rel" "../up" "~/x" "a/b"]]
    (testing (str variable "=" (pr-str value))
      (is (= fallback (f (env {"HOME" "/h" variable value})))))))

(deftest a-base-directory-never-names-the-file-system-root-for-an-empty-value
  (doseq [[f variable _] homes]
    (is (not= "/" (f (env {"HOME" "/h" variable ""}))))
    (is (str/starts-with? (f (env {"HOME" "/h" variable ""})) "/h/"))))

(deftest an-absolute-base-directory-needs-no-home
  (is (= "/x" (paths/config-home (env {"XDG_CONFIG_HOME" "/x"} {})))))

(deftest a-value-holding-NUL-is-ignored
  (is (= "/h/.config" (paths/config-home (env {"HOME" "/h" "XDG_CONFIG_HOME" (str "/x" (char 0) "y")})))))

(deftest base-directories-of-the-real-environment-are-absolute
  (doseq [f [paths/config-home paths/data-home paths/state-home paths/cache-home]]
    (is (fs/absolute? (f)))))

(deftest runtime-dir-has-no-default
  (is (= "/run/user/1000" (paths/runtime-dir (env {"XDG_RUNTIME_DIR" "/run/user/1000"}))))
  (doseq [value [nil "" " " "run/user/1000"]]
    (is (nil? (paths/runtime-dir (env {"HOME" "/h" "XDG_RUNTIME_DIR" value}))))))

(deftest config-dirs-and-data-dirs-keep-absolute-entries-in-order
  (is (= ["/a" "/b"] (paths/config-dirs (env {"XDG_CONFIG_DIRS" "/a:/b"}))))
  (is (= ["/a" "/b"] (paths/config-dirs (env {"XDG_CONFIG_DIRS" "/a::rel:/b/::./x"}))))
  (is (= ["/a"] (paths/data-dirs (env {"XDG_DATA_DIRS" "/a:/a/"}))))
  (is (= ["/b" "/a"] (paths/data-dirs (env {"XDG_DATA_DIRS" "/b:/a"})))))

(deftest config-dirs-and-data-dirs-fall-back-when-nothing-valid-is-left
  (doseq [value [nil "" ":" " " "rel:other" "::"]]
    (testing (pr-str value)
      (is (= ["/etc/xdg"] (paths/config-dirs (env {"XDG_CONFIG_DIRS" value}))))
      (is (= ["/usr/local/share" "/usr/share"] (paths/data-dirs (env {"XDG_DATA_DIRS" value})))))))

(deftest unknown-options-are-refused
  (is (= ::paths/invalid-options (refusal #(paths/config-home {:get-env identity}))))
  (is (= ::paths/invalid-options (refusal #(paths/config-home {:getenv "x"}))))
  (is (= ::paths/invalid-options (refusal #(paths/config-home [:getenv]))))
  (is (= ::paths/invalid-options (refusal #(paths/config-home {:home-paths []})))))

(def ^:private env-value
  (gen/one-of [(gen/return nil)
               (gen/elements ["" " " "\t" "/" "/x" "/x/" "x" "./x" "~" "~/x" "/a//b"])
               gen/string-ascii
               (gen/fmap #(str "/" %) gen/string-alphanumeric)]))

(deftest property-a-base-directory-is-the-value-when-absolute-else-the-fallback
  (holds? 500
          (prop/for-all [value env-value]
            (let [result (paths/state-home (env {"HOME" "/h" "XDG_STATE_HOME" value}))
                  absolute? (and (string? value) (not (str/blank? value))
                                 (not (str/includes? value (str (char 0))))
                                 (str/starts-with? value "/"))]
              (and (str/starts-with? result "/")
                   (if absolute?
                     (= result (str (fs/path value)))
                     (= result "/h/.local/state")))))))

(deftest property-dir-lists-hold-only-absolute-paths
  (holds? 300
          (prop/for-all [entries (gen/vector env-value 0 6)]
            (let [value (str/join ":" (map str entries))
                  dirs (paths/data-dirs (env {"XDG_DATA_DIRS" value}))]
              (and (seq dirs)
                   (every? #(str/starts-with? % "/") dirs)
                   (= dirs (distinct dirs)))))))

;; expand-home

(deftest expand-home-replaces-a-leading-tilde
  (let [opts (env {"HOME" "/h"})]
    (is (= "/h" (paths/expand-home "~" opts)))
    (is (= "/h/" (paths/expand-home "~/" opts)))
    (is (= "/h/x/y" (paths/expand-home "~/x/y" opts)))))

(deftest expand-home-leaves-other-strings-alone
  (let [opts (env {} {})]
    (doseq [s ["~user" "~user/x" "~~" "x/~" "/a/~/b" "" " ~" "./~" "/abs" "rel"]]
      (testing (pr-str s)
        (is (= s (paths/expand-home s opts)))))))

(deftest expand-home-passes-nil-and-refuses-other-types
  (is (nil? (paths/expand-home nil)))
  (is (= ::paths/invalid-input (refusal #(paths/expand-home :x))))
  (is (= ::paths/invalid-input (refusal #(paths/expand-home 1)))))

(deftest expand-home-looks-up-home-only-when-needed
  (is (= "/abs" (paths/expand-home "/abs" (env {} {}))))
  (is (= ::paths/no-home (refusal #(paths/expand-home "~/x" (env {} {}))))))

(deftest property-expand-home-changes-only-tilde-forms
  (holds? 500
          (prop/for-all [s gen/string-ascii]
            (let [out (paths/expand-home s (env {"HOME" "/h"}))]
              (if (or (= "~" s) (str/starts-with? s "~/"))
                (= out (str "/h" (subs s 1)))
                (= out s))))))

;; load-edn-config

(def ^:private config-defaults
  {:store "~/store" :dirs ["~/a" "/b"] :completion {:bash "~/c" :n 1} :keep 3 :name "~user"})

(deftest a-missing-file-yields-the-defaults-expanded
  (with-temp-dir [dir]
    (is (= {:store "/h/store" :dirs ["/h/a" "/b"] :completion {:bash "/h/c" :n 1} :keep 3 :name "~user"}
           (paths/load-edn-config (str dir "/none.edn") config-defaults
                                  (assoc (env {"HOME" "/h"}) :home-paths [[:store] [:dirs] [:completion] [:name] [:absent]]))))))

(deftest the-file-replaces-default-keys-one-for-one
  (with-temp-dir [dir]
    (let [f (str dir "/c.edn")]
      (spit f "{:keep 5 :completion {:zsh \"~/z\"} :extra true}")
      (is (= {:store "~/store" :dirs ["~/a" "/b"] :completion {:zsh "/h/z"} :keep 5 :name "~user" :extra true}
             (paths/load-edn-config f config-defaults (assoc (env {"HOME" "/h"}) :home-paths [[:completion]])))))))

(deftest load-edn-config-without-home-paths-expands-nothing
  (with-temp-dir [dir]
    (is (= config-defaults (paths/load-edn-config (str dir "/none.edn") config-defaults)))))

(deftest load-edn-config-can-refuse-a-missing-file
  (with-temp-dir [dir]
    (is (= ::paths/missing (refusal #(paths/load-edn-config (str dir "/none.edn") {} {:missing :refuse}))))))

(deftest load-edn-config-refuses-what-is-not-one-map
  (with-temp-dir [dir]
    (let [f (str dir "/c.edn")]
      (doseq [[text type] [["[1 2]" :briko.edn/unexpected]
                           ["{:a 1} {:b 2}" :briko.edn/trailing-form]
                           ["" :briko.edn/no-form]
                           ["{:a #inst \"2026-01-01\"}" :briko.edn/tag-refused]
                           [(str (str/join (repeat 100 "{:a ")) (str/join (repeat 100 "}"))) :briko.edn/too-deep]]]
        (spit f text)
        (testing text
          (is (= type (refusal #(paths/load-edn-config f {})))))))))

(deftest load-edn-config-passes-briko-edn-bounds
  (with-temp-dir [dir]
    (let [f (str dir "/c.edn")]
      (spit f "{:a \"0123456789\"}")
      (is (= :briko.edn/too-large (refusal #(paths/load-edn-config f {} {:edn {:max-bytes 8}}))))
      (is (= :briko.edn/unexpected (refusal #(paths/load-edn-config f {} {:edn {:expect :b}}))))
      (is (= {:a "0123456789"} (paths/load-edn-config f {} {:edn {:expect :a}})))
      (is (= ::paths/invalid-options (refusal #(paths/load-edn-config f {} {:edn {:one-form? false}})))))))

(deftest load-edn-config-refuses-a-dangling-link-and-a-directory
  (with-temp-dir [dir]
    (let [link (str dir "/link.edn")]
      (fs/create-sym-link link (str dir "/nowhere.edn"))
      (is (= :briko.edn/not-a-file (refusal #(paths/load-edn-config link {}))))
      (is (= :briko.edn/not-a-file (refusal #(paths/load-edn-config dir {})))))))

(deftest load-edn-config-checks-its-arguments
  (is (= ::paths/invalid-input (refusal #(paths/load-edn-config nil {}))))
  (is (= ::paths/invalid-input (refusal #(paths/load-edn-config "/x" nil))))
  (is (= ::paths/invalid-options (refusal #(paths/load-edn-config "/x" {} {:home-paths [:a]}))))
  (is (= ::paths/invalid-options (refusal #(paths/load-edn-config "/x" {} {:missing :ignore}))))
  (is (= ::paths/invalid-options (refusal #(paths/load-edn-config "/x" {} {:edn 1})))))
