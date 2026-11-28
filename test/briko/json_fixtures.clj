;; SPDX-FileCopyrightText: The briko Authors
;; SPDX-License-Identifier: 0BSD

(ns briko.json-fixtures
  "Generates JSON documents shaped like the ones briko.json reads in
   practice, from a seed, so tests and benchmarks need no recorded files:
   a kubectl pod list, an npm registry document, a GitHub release list,
   and a map shaped like tik's derivation cache for writing. Each is full
   of UUIDs, hex digests and escapes, and holds no number that could be
   refused."
  (:import (java.util Random UUID)))

(set! *warn-on-reflection* true)

(def ^:private hex-digits "0123456789abcdef")

(defn- hex
  [^Random r n]
  (let [sb (StringBuilder.)]
    (dotimes [_ n] (.append sb (.charAt ^String hex-digits (.nextInt r 16))))
    (str sb)))

(defn- uuid
  [^Random r]
  (str (UUID. (.nextLong r) (.nextLong r))))

(defn- word
  [^Random r]
  (nth ["api" "web" "worker" "cache" "db" "proxy" "gateway" "metrics" "e2e" "e10" "exp"]
       (.nextInt r 11)))

(defn- timestamp
  [^Random r]
  (format "2026-%02d-%02dT%02d:%02d:%02dZ"
          (inc (.nextInt r 12)) (inc (.nextInt r 28)) (.nextInt r 24) (.nextInt r 60) (.nextInt r 60)))

(defn pod
  [^Random r i]
  (let [app (word r)
        image (str "registry.example.org/" app ":v1." (.nextInt r 30) "@sha256:" (hex r 64))]
    {"apiVersion" "v1"
     "kind" "Pod"
     "metadata" {"name" (str app "-" (hex r 10) "-" (hex r 5))
                 "namespace" (str "team-" (word r))
                 "uid" (uuid r)
                 "resourceVersion" (str (+ 100000 (.nextInt r 900000)))
                 "creationTimestamp" (timestamp r)
                 "generateName" (str app "-" (hex r 10) "-")
                 "labels" {"app.kubernetes.io/name" app
                           "pod-template-hash" (hex r 10)
                           "controller-revision-hash" (str app "-" (hex r 9))}
                 "annotations" {"checksum/config" (hex r 64)
                                "kubectl.kubernetes.io/restartedAt" (timestamp r)}
                 "ownerReferences" [{"apiVersion" "apps/v1" "kind" "ReplicaSet"
                                     "name" (str app "-" (hex r 10)) "uid" (uuid r)
                                     "controller" true "blockOwnerDeletion" true}]}
     "spec" {"containers" [{"name" app
                            "image" image
                            "args" ["--port=8080" (str "--id=" (hex r 12)) "--log-level=info"]
                            "ports" [{"containerPort" 8080 "protocol" "TCP"}
                                     {"containerPort" (+ 9000 (.nextInt r 100)) "name" "metrics" "protocol" "TCP"}]
                            "resources" {"limits" {"cpu" "500m" "memory" "256Mi"}
                                         "requests" {"cpu" "100m" "memory" "128Mi"}}
                            "env" [{"name" "TOKEN_FILE" "value" "/var/run/secrets/token"}
                                   {"name" "BUILD_ID" "value" (hex r 40)}]}]
             "nodeName" (str "node-" (.nextInt r 8))
             "priority" 0
             "terminationGracePeriodSeconds" 30}
     "status" {"phase" "Running"
               "podIP" (str "10.244." (.nextInt r 255) "." (.nextInt r 255))
               "startTime" (timestamp r)
               "qosClass" "Burstable"
               "conditions" (vec (for [t ["Initialized" "Ready" "ContainersReady" "PodScheduled"]]
                                   {"type" t "status" "True" "lastTransitionTime" (timestamp r)}))
               "containerStatuses" [{"name" app "ready" true "restartCount" (.nextInt r 5)
                                     "image" image
                                     "imageID" (str "registry.example.org/" app "@sha256:" (hex r 64))
                                     "containerID" (str "containerd://" (hex r 64))
                                     "started" true}]
               "index" i}}))

(defn pod-list
  "A `kubectl get pods -A -o json` listing of n pods."
  [seed n]
  (let [r (Random. seed)]
    {"apiVersion" "v1" "kind" "List" "metadata" {"resourceVersion" ""}
     "items" (mapv (partial pod r) (range n))}))

(defn npm-document
  "An npm registry document of n versions."
  [seed n]
  (let [r (Random. seed)
        versions (for [i (range n)] (str "1." (quot i 10) "." (mod i 10)))]
    {"_id" "example-package"
     "_rev" (str (inc (.nextInt r 900)) "-" (hex r 32))
     "name" "example-package"
     "description" "An example package with a déjà-vu description – and emoji 🚀"
     "dist-tags" {"latest" (last versions)}
     "time" (into {"created" (timestamp r) "modified" (timestamp r)}
                  (map (fn [v] [v (timestamp r)]) versions))
     "versions" (into {}
                      (map (fn [v]
                             [v {"name" "example-package"
                                 "version" v
                                 "main" "index.js"
                                 "license" "MIT"
                                 "gitHead" (hex r 40)
                                 "dependencies" {"left-pad" (str "^1." (.nextInt r 9) ".0")
                                                 "e2e-utils" "~3.1.4"}
                                 "dist" {"shasum" (hex r 40)
                                         "integrity" (str "sha512-" (hex r 86) "==")
                                         "tarball" (str "https://registry.example.org/example-package/-/example-package-" v ".tgz")
                                         "fileCount" (.nextInt r 200)
                                         "unpackedSize" (.nextInt r 2000000)
                                         "signatures" [{"keyid" (str "SHA256:" (hex r 43))
                                                        "sig" (str "MEUCI" (hex r 90))}]}
                                 "_npmVersion" "10.8.1"
                                 "_nodeVersion" "22.3.0"
                                 "deprecated" nil}]))
                      versions)}))

(defn release-list
  "A GitHub list of n releases, each with a few assets."
  [seed n]
  (let [r (Random. seed)]
    (vec (for [i (range n)]
           (let [tag (str "v2026." (inc (mod i 12)) "." (inc (mod i 28)))]
             {"url" (str "https://api.example.org/repos/o/r/releases/" (+ 100000000 i))
              "id" (+ 100000000 i)
              "node_id" (str "RE_kwDO" (hex r 14))
              "tag_name" tag
              "target_commitish" (hex r 40)
              "name" tag
              "draft" false
              "prerelease" (zero? (.nextInt r 5))
              "created_at" (timestamp r)
              "published_at" (timestamp r)
              "author" {"login" "release-bot" "id" 41898282 "node_id" (str "MDM6Qm90" (hex r 10)) "type" "Bot"}
              "body" (str "## Changes\n\n* fix: handle \"quoted\" names — " (hex r 7)
                          "\n* feat: e2e tests at 1e3 req/s (see #" (.nextInt r 500) ")\n")
              "assets" (vec (for [os ["linux" "darwin" "windows"] arch ["amd64" "arm64"]]
                              {"id" (+ 200000000 (.nextInt r 100000000))
                               "node_id" (str "RA_kwDO" (hex r 14))
                               "name" (str "tool-" os "-" arch ".tar.gz")
                               "content_type" "application/gzip"
                               "state" "uploaded"
                               "size" (+ 5000000 (.nextInt r 5000000))
                               "digest" (str "sha256:" (hex r 64))
                               "download_count" (.nextInt r 10000)
                               "created_at" (timestamp r)
                               "browser_download_url" (str "https://example.org/o/r/releases/download/" tag "/tool-" os "-" arch ".tar.gz")}))})))))

(defn derivation-cache
  "A map shaped like tik's .derived-cache.json: n ticket ids, each with a
   fingerprint and a row whose keys and keyword values are \":kw\" strings."
  [seed n]
  (let [r (Random. seed)]
    (into {}
          (for [_ (range n)]
            [(uuid r)
             {":fp" (hex r 64)
              ":valid-until" nil
              ":row" {":haystack" (str "change " (word r) " " (hex r 8) " https://github.com/metio/x/pull/" (.nextInt r 500)
                                       " docker.io/library/x:v1@sha256:" (hex r 64) " :merged")
                      ":reached" [":scoped" ":proposed" ":proven"]
                      ":settled?" (.nextBoolean r)
                      ":last-event-ms" (+ 1785000000000 (.nextInt r 1000000000))
                      ":current" [":proven"]
                      ":repo" (str ":" (word r))
                      ":process-id" ":change"
                      ":title" (str "briko: " (word r) " – " (hex r 6))
                      ":links" []
                      ":describe" nil
                      ":depth" (.nextInt r 6)}}]))))
