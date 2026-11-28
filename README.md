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
- it depends on nothing beyond Clojure, [babashka.fs](https://github.com/babashka/fs) and [babashka.process](https://github.com/babashka/process). babashka carries both built in; on the JVM they arrive through briko's `deps.edn`. The JSON functions of `briko.mcp` resolve [cheshire](https://github.com/dakrone/cheshire) when first called: babashka carries it, a JVM consumer declares it, and a native image requires `cheshire.core` where it is built.

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

- `briko.sshsig` verifies OpenSSH detached signatures (the SSHSIG format of `ssh-keygen -Y sign`) in process, against allowed-signers text, for `ssh-ed25519` keys and Ed25519 security keys. A security-key signature counts only when the key reports a touch. Other key types, unknown hashes and allowed-signers lines with `valid-before`/`valid-after` are answered `:briko.sshsig/unsupported` instead of false.

```clojure
(require '[briko.sshsig :as sshsig])

(sshsig/verify allowed-signers-text message-bytes armored-signature "alice" "my-app-event")
;; => true, false or :briko.sshsig/unsupported
```

- `briko.ssh-sign` signs bytes through `ssh-keygen -Y sign` and verifies them: in process through `briko.sshsig` where it can, through `ssh-keygen -Y verify` for the key types it cannot judge. A verdict is true, false, or nil when neither could judge. The signing namespace is always an explicit argument.

```clojure
(require '[briko.ssh-sign :as ssh-sign])

(def sig (ssh-sign/sign-bytes "/path/to/key" message-bytes "my-app-event"))
(ssh-sign/verify-bytes "/path/to/allowed_signers" message-bytes sig "alice" "my-app-event")
(ssh-sign/fingerprint (ssh-sign/pubkey "/path/to/key"))  ;; => "SHA256:..."
```

- `briko.mcp` handles the messages of a Model Context Protocol server, whatever carries them: the legacy `initialize` handshake and the modern revisions whose every request names its version and capabilities in `_meta`, the header check of modern Streamable HTTP requests, and request dispatch over handlers and a tool registry the caller supplies. A notification gets no response, and a failing handler answers with a JSON-RPC error instead of ending the session. `serve-stdio` answers over stdin and stdout; the HTTP server stays with the caller.

```clojure
(require '[briko.mcp :as mcp])

(def server
  (mcp/server {:server-info {:name "demo" :version "1"}
               :capabilities {:tools {}}
               :handlers (mcp/tool-handlers
                          [{:name "echo" :description "Echoes text." :inputSchema {:type "object"}
                            :call (fn [args _request] {:text (get args "text")})}])}))

(let [{:keys [response status]} (mcp/handle-line server line {:headers lower-cased-headers})]
  (when response (mcp/write-json response)))
```

- `briko.skill-manifest` encodes, signs and verifies the `skill-manifest-v1` manifest of an agent skill served through the MCP Skills extension. A manifest names one skill, lists its SKILL.md, and holds only `skill://` URIs without `.` or `..` segments. The signature namespace and the size limits are arguments of every call.

```clojure
(require '[briko.skill-manifest :as sm])

(def limits {:max-files 512 :max-file-bytes 100000 :max-skill-bytes 400000})
(def marker (sm/sign entry {:key-file "/path/to/key" :actor "alice" :namespace "demo-skill-manifest" :limits limits}))
(sm/verify allowed-signers-text (assoc entry :_meta {sm/meta-key marker})
           {:namespaces #{"demo-skill-manifest"} :limits limits})
;; => {:verdict :verified :actor "alice" :namespace "demo-skill-manifest"}
```

## Cryptography

briko implements no cryptographic primitive. Hashes and signature checks are the JDK's (`java.security`: SHA-256, SHA-512, Ed25519), and signing is OpenSSH's (`ssh-keygen`), so briko never reads a private key. What briko implements itself is the parsing of the SSHSIG wire format and of allowed-signers lines, and the assembly of the bytes a signature covers. Its tests check those verdicts against `ssh-keygen` on signatures `ssh-keygen` made.

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
