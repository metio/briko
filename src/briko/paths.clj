;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.paths
  "Where a program keeps its files: the XDG base directories, the home
   directory, `~` in configured paths, and an EDN configuration file read
   over defaults.

   The base directories follow the XDG Base Directory Specification 0.8.
   A variable that is unset, empty, blank or relative counts as unset,
   because the specification calls a relative path invalid and tells
   implementations to ignore it; the specification's default under the
   home directory applies instead. A list variable (XDG_CONFIG_DIRS,
   XDG_DATA_DIRS) drops its empty and relative entries one by one and
   falls back to its default when none is left. Every path is returned
   as a string as java.nio.file.Path prints it, without a trailing or
   doubled slash; `.` and `..` segments are kept, because resolving them
   without the file system can change which directory a path names.

   The home directory is $HOME when that is an absolute path, else the
   user.home system property, which the JVM and babashka take from the
   password database, when that is absolute. With neither, a function
   that needs the home directory refuses with ::no-home rather than
   resolving paths against the working directory or the file system
   root. Functions that do not need it, such as `config-home` with an
   absolute XDG_CONFIG_HOME, never look it up.

   Every function takes an optional map of options:

     :getenv   a function from a variable name to its value or nil;
               System/getenv by default
     :getprop  a function from a system property name to its value or
               nil; System/getProperty by default

   Every refusal is an ex-info whose data carries a stable :type, one of
   `refusal-types`. `load-edn-config` also passes on briko.edn's
   refusals, which carry the :file."
  (:require [babashka.fs :as fs]
            [briko.edn :as edn]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def defaults
  "The environment the functions read where the caller gives no options."
  {:getenv (fn [n] (System/getenv ^String n))
   :getprop (fn [n] (System/getProperty ^String n))})

(def refusal-types
  "Every :type a refusal carries."
  #{::invalid-options ::invalid-input ::no-home ::missing})

(defn- refuse!
  ([type message data] (refuse! type message data nil))
  ([type message data cause]
   (throw (ex-info message (assoc data :type type) cause))))

(defn refusal?
  "Whether e is a refusal of this namespace."
  [e]
  (contains? refusal-types (:type (ex-data e))))

(defn- env-options
  [opts allowed]
  (when-not (or (nil? opts) (map? opts))
    (refuse! ::invalid-options "the options must be a map" {}))
  (when-let [unknown (seq (remove allowed (keys opts)))]
    (refuse! ::invalid-options (str "unknown options " (pr-str (vec unknown))) {:unknown (vec unknown)}))
  (let [opts (merge defaults opts)]
    (doseq [k [:getenv :getprop]]
      (when-not (ifn? (get opts k))
        (refuse! ::invalid-options (str k " must be a function") {:option k})))
    opts))

(def ^:private env-keys (set (keys defaults)))

(defn- absolute
  "s as a Path prints it when it is a non-blank absolute path, else nil."
  [s]
  (when (and (string? s) (not (str/blank? s)) (neg? (.indexOf ^String s (int 0))))
    (let [p (fs/path s)]
      (when (fs/absolute? p)
        (str p)))))

(defn- home*
  [{:keys [getenv getprop]}]
  (or (absolute (getenv "HOME"))
      (absolute (getprop "user.home"))
      (refuse! ::no-home
               "no home directory: $HOME is not an absolute path and neither is the user.home property"
               {})))

(defn home
  "The home directory: $HOME when it is an absolute path, else the
   user.home property when that is. Refuses with ::no-home otherwise."
  ([] (home nil))
  ([opts] (home* (env-options opts env-keys))))

(defn- under-home
  [opts & segments]
  (str (apply fs/path (home* opts) segments)))

(defn- base-dir
  [opts variable & default-segments]
  (let [opts (env-options opts env-keys)]
    (or (absolute ((:getenv opts) variable))
        (apply under-home opts default-segments))))

(defn config-home
  "$XDG_CONFIG_HOME, or ~/.config when it is unset, blank or relative."
  ([] (config-home nil))
  ([opts] (base-dir opts "XDG_CONFIG_HOME" ".config")))

(defn data-home
  "$XDG_DATA_HOME, or ~/.local/share when it is unset, blank or relative."
  ([] (data-home nil))
  ([opts] (base-dir opts "XDG_DATA_HOME" ".local" "share")))

(defn state-home
  "$XDG_STATE_HOME, or ~/.local/state when it is unset, blank or relative."
  ([] (state-home nil))
  ([opts] (base-dir opts "XDG_STATE_HOME" ".local" "state")))

(defn cache-home
  "$XDG_CACHE_HOME, or ~/.cache when it is unset, blank or relative."
  ([] (cache-home nil))
  ([opts] (base-dir opts "XDG_CACHE_HOME" ".cache")))

(defn runtime-dir
  "$XDG_RUNTIME_DIR, or nil when it is unset, blank or relative. The
   specification defines no default: a directory owned by the user, mode
   0700, removed at logout, has no equivalent under the home directory,
   so a caller without one picks its own replacement and should warn."
  ([] (runtime-dir nil))
  ([opts]
   (let [opts (env-options opts env-keys)]
     (absolute ((:getenv opts) "XDG_RUNTIME_DIR")))))

(defn- dir-list
  [opts variable default]
  (let [opts (env-options opts env-keys)
        value ((:getenv opts) variable)
        dirs (when (string? value)
               (into [] (keep absolute) (str/split value #":")))]
    (if (seq dirs) (vec (distinct dirs)) default)))

(defn config-dirs
  "The absolute entries of $XDG_CONFIG_DIRS in order of preference, or
   [\"/etc/xdg\"] when it is unset or holds none."
  ([] (config-dirs nil))
  ([opts] (dir-list opts "XDG_CONFIG_DIRS" ["/etc/xdg"])))

(defn data-dirs
  "The absolute entries of $XDG_DATA_DIRS in order of preference, or
   [\"/usr/local/share\" \"/usr/share\"] when it is unset or holds none."
  ([] (data-dirs nil))
  ([opts] (dir-list opts "XDG_DATA_DIRS" ["/usr/local/share" "/usr/share"])))

(defn- expand-home*
  [s opts]
  (cond
    (nil? s) nil
    (not (string? s)) (refuse! ::invalid-input (str "the path must be a string, not a " (.getName (class s))) {})
    (= "~" s) (home* opts)
    (str/starts-with? s "~/") (str (home* opts) (subs s 1))
    :else s))

(defn expand-home
  "s with a leading `~` replaced by the home directory, when s is `~` or
   starts with `~/`. Any other string, `~user/x` included, is returned as
   it is: a `~user` form names another user's home, which only the
   password database knows. nil passes through."
  ([s] (expand-home s nil))
  ([s opts] (expand-home* s (env-options opts env-keys))))

;; Configuration files

(def ^:private config-keys
  (into env-keys #{:edn :home-paths :missing}))

(defn- expand-value
  [opts v]
  (cond
    (string? v) (expand-home* v opts)
    (map? v) (update-vals v #(if (string? %) (expand-home* % opts) %))
    (vector? v) (mapv #(if (string? %) (expand-home* % opts) %) v)
    (or (seq? v) (set? v)) (into (empty v) (map #(if (string? %) (expand-home* % opts) %)) v)
    :else v))

(defn- expand-paths
  [config home-paths opts]
  (reduce (fn [m ks]
            (if (some? (get-in m ks))
              (update-in m ks (partial expand-value opts))
              m))
          config
          home-paths))

(defn- check-config-options!
  [path defaults-map {:keys [edn home-paths missing]}]
  (when-not (or (string? path) (instance? java.nio.file.Path path) (instance? java.io.File path))
    (refuse! ::invalid-input "the configuration path must be a string, Path or File" {}))
  (when-not (map? defaults-map)
    (refuse! ::invalid-input "the defaults must be a map" {}))
  (when-not (or (nil? edn) (map? edn))
    (refuse! ::invalid-options ":edn must be a map of briko.edn options" {:option :edn}))
  (when (contains? edn :one-form?)
    (refuse! ::invalid-options ":edn must not set :one-form?; a configuration file holds one map" {:option :edn}))
  (when-not (and (or (nil? home-paths) (sequential? home-paths) (set? home-paths))
                 (every? #(and (vector? %) (seq %)) home-paths))
    (refuse! ::invalid-options ":home-paths must be a collection of non-empty key vectors" {:option :home-paths}))
  (when-not (contains? #{nil :defaults :refuse} missing)
    (refuse! ::invalid-options ":missing must be :defaults or :refuse" {:option :missing})))

(defn load-edn-config
  "The configuration in the EDN file at path, a map, merged over the map
   defaults-map, with `~` expanded in the values :home-paths names.

   The file is read through briko.edn with the :edn options (briko.edn's
   defaults where absent) and must hold exactly one map; a caller's
   :expect must hold as well. Its keys replace those of defaults-map one
   for one, as `merge` does: a nested map in the file replaces the whole
   default map under that key.

   Options, besides :getenv and :getprop:

     :edn         briko.edn options for the read
     :home-paths  key vectors such as [[:store] [:completion-dirs]]; the
                  value under each, after the merge, has `expand-home`
                  applied when it is a string, and to each string inside
                  it when it is a vector, list, set or map (one level)
     :missing     :defaults (default) returns defaults-map, expanded,
                  when nothing exists at path; :refuse refuses with
                  ::missing

   A dangling symbolic link at path is not missing: briko.edn refuses it
   as not a regular file."
  ([path defaults-map] (load-edn-config path defaults-map nil))
  ([path defaults-map opts]
   (let [opts (env-options opts config-keys)
         _ (check-config-options! path defaults-map opts)
         {:keys [edn home-paths missing]} opts
         present? (fs/exists? path {:nofollow-links true})]
     (when (and (not present?) (= :refuse missing))
       (refuse! ::missing (str "the configuration file " path " does not exist") {:file (str path)}))
     (let [expect (:expect edn)
           user (when present?
                  (edn/read-file path (assoc edn :expect (if expect
                                                           (fn [x] (and (map? x) (expect x)))
                                                           map?))))]
       (expand-paths (merge defaults-map user) home-paths opts)))))
