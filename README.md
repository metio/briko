<!--
SPDX-FileCopyrightText: The briko Authors
SPDX-License-Identifier: 0BSD
-->

# briko

briko is metio's shared Clojure library. Each namespace is one small, self-contained brick that does a single job from explicit inputs, so projects compose the bricks they need instead of carrying their own copies. *Briko* is Esperanto for brick.

## Depending on briko

briko is consumed as a [tools.deps git dependency](https://clojure.org/reference/deps_edn#deps_git). There is no Maven artifact. Every release is a calendar-versioned tag such as `2026.11.27143022`; pin both the tag and its commit:

```clojure
;; deps.edn or bb.edn
{:deps {io.github.metio/briko {:git/url "https://github.com/metio/briko"
                               :git/tag "2026.11.27143022"
                               :git/sha "<the commit the tag points at>"}}}
```

The repository is public, so resolving it needs no credentials. The `:git/sha` pins the content, and Renovate can bump both values together.

## Runtime constraints

Every namespace runs under babashka, on the JVM and in a GraalVM native image:

- it uses only classes babashka exposes;
- it compiles without reflection warnings, because a reflective call fails at run time in a native image;
- it depends on nothing beyond Clojure, [babashka.fs](https://github.com/babashka/fs) and [babashka.process](https://github.com/babashka/process). babashka carries both built in; on the JVM they arrive through briko's `deps.edn`.

A function that cannot do its job cleanly throws an `ex-info` whose data carries a stable `:type` keyword, so callers can branch on the failure without parsing messages.

## Namespaces

- `briko.edn` reads EDN from text that may come from anywhere: within a byte limit, valid UTF-8 only, with nesting refused before the recursive reader sees it, every tagged literal refused unless the caller supplies its reader, and exactly one form unless asked otherwise.

```clojure
(require '[briko.edn :as edn])

(edn/read-file ".tool.edn" {:max-bytes 65536 :expect map?})
(edn/read-string text {:readers {'inst #(java.time.Instant/parse %)}})
```

- `briko.process` runs a program as an argv, never through a shell, within a deadline (60 s by default) and with stdout and stderr captured up to a byte limit (1 MiB and 64 KiB). It never throws for a program that cannot be started (exit 127) or that overruns its deadline (exit 124, after its process tree was killed).

```clojure
(require '[briko.process :as process])

(process/run ["git" "status" "--porcelain"] {:dir repo :timeout-ms 10000 :unset-env ["GIT_DIR"]})
;; => {:exit 0 :out "..." :err "" :timed-out? false :launched? true
;;     :out-truncated? false :err-truncated? false}
```

- `briko.secret` resolves secrets named in configuration at the moment of use: `{:env "NAME"}`, `{:file "/path"}`, `{:credential "NAME"}` (systemd credentials) and `{:command ["pass" "show" "x"]}`. An unset variable, a missing file, an empty or blank value and a command that fails or overruns its deadline are refused; no refusal carries the secret or the command's output.

```clojure
(require '[briko.secret :as secret])

(secret/resolve-secrets nil {:url "https://example.org/hook"
                             :headers {"Authorization" {:command ["pasejo" "secret" "show" "--line" "1" "api/x"]}}})
(secret/materialize-file "ssh key" {:credential "signing-key"} private-dir "signing-key")
```

## Development

The toolchain comes from [breto](https://github.com/metio/breto): run `eval "$(breto env)"` in the checkout, then

```shell
bb test        # the test suite on the JVM (kaocha) and under babashka
bb lint        # clj-kondo, failing on warnings
bb analyze     # eastwood and splint
bb fmt         # cljfmt check; `bb fmt fix` rewrites
bb reflection  # load every namespace on the JVM and fail on a reflection warning
reuse lint     # licence headers
```

## License

briko is licensed under [0BSD](LICENSE).
