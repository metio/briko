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
- it depends on nothing beyond Clojure, [babashka.fs](https://github.com/babashka/fs) and [babashka.process](https://github.com/babashka/process). babashka carries both built in; on the JVM they arrive through briko's `deps.edn`. `briko.json`, and `briko.mcp` through it, resolve [cheshire](https://github.com/dakrone/cheshire) when first called: babashka carries it, a JVM consumer declares it, and a native image requires `cheshire.core` and `cheshire.factory` where it is built.

A function that cannot do its job cleanly throws an `ex-info` whose data carries a stable `:type` keyword, so callers can branch on the failure without parsing messages.

## Namespaces

- `briko.edn` reads EDN from text that may come from anywhere: within a byte limit, valid UTF-8 only, with nesting refused before the recursive reader sees it, every tagged literal refused unless the caller supplies its reader, and exactly one form unless asked otherwise.

```clojure
(require '[briko.edn :as edn])

(edn/read-file ".tool.edn" {:max-bytes 65536 :expect map?})
(edn/read-string text {:readers {'inst #(java.time.Instant/parse %)}})
```

- `briko.json` reads JSON from text that may come from anywhere, strictly: RFC 8259 only (no comments, NaN, trailing commas or raw control characters), within a byte limit, valid UTF-8 only, at most 64 levels deep, with number literals of at most 100 characters and no decimal that overflows a double, no object that names a key twice, and exactly one value unless the caller asks for every value of the text, as of JSON Lines. Keys stay strings unless the caller asks for keywords, for every key or only for a set of names. `write-string` prints JSON with every control, format, separator, lone-surrogate, unassigned and default-ignorable code point written as `\uXXXX`, so nothing invisible or terminal-controlling reaches a reader raw while readable text such as `Größe` stays as it is, and refuses a map whose keys print to the same name. Pretty output takes an indentation of 0 to 16 spaces (`:indent`), one line per array element (`:indent-arrays?`) and a key separator of a colon alone, a colon and a space, or a colon between two spaces (`:key-separator`, one of `briko.json/key-separators`), byte-identical to cheshire's printer with the same options. `:escape-hidden? false` leaves hidden code points raw, as plain cheshire writes them, for bytes a format fixes such as an RFC 7638 thumbprint; such output must not reach a terminal or an agent. No refusal carries the input; a position is given as `:line` and `:column`.

```clojure
(require '[briko.json :as json])

(json/parse-string "{\"items\":[1,2]}" {:keys #{"items"} :max-bytes 65536})  ;; => {:items [1 2]}
(json/write-string {:name "evil\u202e"})                                        ;; => "{\"name\":\"evil\\u202E\"}"
(json/write-string {:a [1]} {:indent 2 :key-separator ": " :indent-arrays? true}) ;; => "{\n  \"a\": [\n    1\n  ]\n}"
```

- `briko.process` runs a program as an argv, never through a shell, within a deadline (60 s by default) and with stdout and stderr captured up to a byte limit (1 MiB and 64 KiB). It never throws for a program that cannot be started (exit 127) or that overruns its deadline (exit 124, after its process tree was killed). Its output travels through named pipes, so `mkfifo` must be on the PATH; a run that finds none answers 127. On a JVM with native access enabled for briko (`--enable-native-access=ALL-UNNAMED`, Java 22 or later), the pipes are created in process through `mkfifo(3)` instead, which saves starting a program per run; without it the JVM would print a warning on the first call, so briko then uses the program.

```clojure
(require '[briko.process :as process])

(process/run ["git" "status" "--porcelain"] {:dir repo :timeout-ms 10000 :unset-env ["GIT_DIR"]})
;; => {:exit 0 :out "..." :err "" :timed-out? false :launched? true
;;     :out-truncated? false :err-truncated? false}
```

  Each relaxation of those defaults is an option the caller names. `:timeout-ms :none` runs without a deadline, for builds and clones; interrupting the waiting thread still kills the tree. `:err-keep :tail` (or `:out-keep :tail`) keeps the last bytes of a stream past its limit instead of the first, where a failed build writes its error. `:out {:file path}` writes stdout into a file, created owner-only or truncated, never through a symbolic link; it still travels through the pipe, so a detached child holding stdout makes the run 124 and nothing reaches the file after `run` returned, and the result carries `:out-bytes` and `:out-error` instead of `:out`. `:in {:file path}` reads stdin from a regular file and `:in :tty` from the controlling terminal, answering 127 where there is none. A limit or keep option for a stream that is not captured is refused.

```clojure
(process/run ["cargo" "build"] {:dir crate :timeout-ms :none :err-keep :tail})
(process/run ["zstd" "--decompress" "--stdout" archive] {:out {:file target} :timeout-ms 600000})
(process/run ["stty" "size"] {:in :tty})
```

  `:on-start` receives `{:pid … :stop (fn [])}` once the program has started. `:stop` kills the program, its tree and every holder of its output as a timeout does, from any thread, so a JVM shutdown hook can end a run in flight; the run then returns the program's exit (137 after SIGKILL) with `:stopped? true`, a key every result of a run with `:on-start` carries. `:stop` is idempotent and does nothing once the run has returned. A callback that throws has the program stopped and `run` refuses with `:briko.process/on-start-failed`.

```clojure
(def running (atom nil))
(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (when-let [h @running] ((:stop h))))))
(process/run ["make" "release"] {:timeout-ms :none :on-start (fn [handle] (reset! running handle))})
```

  `start` runs a program alongside the caller, such as an SSH tunnel or a port-forward, through the same named pipes, and returns a handle `{:pid … :launched? …}`. The program has no deadline; its output is read all the time and only the last 64 KiB of each stream are kept (`:max-out-bytes`, `:max-err-bytes`), or stdout goes to `{:file path}`. `output` returns what was kept, `alive?` whether it runs. `wait-ready` waits until a line of output matches `:pattern` (on `:out` or `:err`) or a `:port` on the loopback interface accepts, within `:timeout-ms` (30 s by default, never unbounded), and optionally that the program still runs `:settle-ms` later; it refuses with `:briko.process/exited-before-ready` (data `:exit`) or `:briko.process/ready-timeout` and closes the program first unless `:close-on-refusal? false`. `close` kills the program, its tree and every holder of its output, is idempotent, and returns the exit (137 after SIGKILL, the program's own when it had exited). One JVM shutdown hook stops every started program still running, unless it was started with `:stop-on-exit? false`. A program that cannot be started, also for want of `mkfifo`, gives a handle with `:launched? false` that `close` answers with 127.

```clojure
(let [tunnel (process/start ["ssh" "-N" "-o" "ExitOnForwardFailure=yes" "-L" (str "127.0.0.1:" port ":forge:443") host])]
  (try
    (process/wait-ready tunnel {:port port :settle-ms 1000 :timeout-ms 15000})
    (call-forge port)
    (finally (process/close tunnel))))
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

- `briko.mcp` handles the messages of a Model Context Protocol server, whatever carries them: the legacy `initialize` handshake and the modern revisions whose every request names its version and capabilities in `_meta`, the header check of modern Streamable HTTP requests, and request dispatch over handlers and a tool registry the caller supplies. A notification gets no response, and a failing handler answers with a JSON-RPC error instead of ending the session. Messages are read and written by `briko.json`: a message is exactly one JSON value within `:max-message-bytes` (16 MiB by default), without duplicate keys, nesting over 64 levels or number literals over 100 characters, and anything else answers -32700 with a message and `data.rule` naming the rule it broke, never the text. `write-response` writes an answer with hidden code points escaped, and answers -32603 for a result JSON cannot hold. `serve-stdio` answers over stdin and stdout and refuses a line over `:max-message-bytes` without holding it whole; the HTTP server stays with the caller.

```clojure
(require '[briko.mcp :as mcp])

(def server
  (mcp/server {:server-info {:name "demo" :version "1"}
               :capabilities {:tools {}}
               :handlers (mcp/tool-handlers
                          [{:name "echo" :description "Echoes text." :inputSchema {:type "object"}
                            :call (fn [args _request] {:text (get args "text")})}])}))

(let [{:keys [response status]} (mcp/handle-line server line {:headers lower-cased-headers
                                                               :max-message-bytes 1048576})]
  (when response (mcp/write-response response)))

(mcp/serve-stdio server {:max-message-bytes 1048576})
```

- `briko.skill-manifest` encodes, signs and verifies the `skill-manifest-v1` manifest of an agent skill served through the MCP Skills extension. A manifest names one skill, lists its SKILL.md, and holds only `skill://` URIs without `.` or `..` segments. The signature namespace and the size limits are arguments of every call. A `:malformed` verdict carries a stable `:code` from `malformed-codes` beside its human-readable `:reason`, so a caller branches on the code.

```clojure
(require '[briko.skill-manifest :as sm])

(def limits {:max-files 512 :max-file-bytes 100000 :max-skill-bytes 400000})
(def marker (sm/sign entry {:key-file "/path/to/key" :actor "alice" :namespace "demo-skill-manifest" :limits limits}))
(sm/verify allowed-signers-text (assoc entry :_meta {sm/meta-key marker})
           {:namespaces #{"demo-skill-manifest"} :limits limits})
;; => {:verdict :verified :actor "alice" :namespace "demo-skill-manifest"}
(sm/verify allowed-signers-text (assoc-in entry [:_meta sm/meta-key "format"] "v2")
           {:namespaces #{"demo-skill-manifest"} :limits limits})
;; => {:verdict :malformed :code :unknown-format :reason "unknown manifest format \"v2\""}
```

- `briko.paths` names where a program keeps its files: the XDG base directories, ignoring a variable that is empty, blank or relative as the specification asks; the home directory (`$HOME` when absolute, else the `user.home` property, else a refusal); `expand-home` for `~` and `~/…`; `abbreviate-home`, its inverse for display, which writes the home directory as `~` on a path boundary only; and `load-edn-config`, which reads one map through `briko.edn` and merges it over defaults. An empty or comment-only file and a dangling symbolic link are refused unless the caller opts in with `:empty :defaults` or `:dangling :missing`.

```clojure
(require '[briko.paths :as paths])

(paths/config-home)                      ;; => "/home/me/.config"
(paths/load-edn-config (str (paths/config-home) "/tool/config.edn")
                       {:store "~/.local/share/tool" :keep 3}
                       {:home-paths [[:store]] :edn {:max-bytes 65536}})
;; an empty file reads as the defaults only when asked for
(paths/load-edn-config path {:keep 3} {:empty :defaults :dangling :missing})
```

- `briko.fs` writes files safely. `atomic-write!` stages the content in a uniquely named file beside the target, forces it to disk and renames it into place, so a reader sees the old content or the new and two writers never mix; it refuses rather than falls back when the file system cannot rename atomically. `:mode` gives the staged file exact permissions (`"rwxr-xr-x"`, or `:keep` for those of the file it replaces) before the rename, so the path never shows the umask's mode; `:symlink :follow` writes the file a symbolic link at the path ends at, within 40 links and optionally inside a `:root`, instead of replacing the link. `replace-symlink!` repoints a symbolic link in one rename, so readers never find the path missing, and refuses a directory and, unless asked, a regular file. `staged?` recognises the names both stage under. `write-private!` and `create-private-directory!` create a file `rw-------` or a directory `rwx------` in the same call that creates it, refuse whatever already exists at the path, a symbolic link included, and refuse a file system that cannot keep the permissions.

```clojure
(require '[briko.fs :as bfs])

(bfs/atomic-write! "state/inventory.edn" (pr-str inventory))
(bfs/atomic-write! "cache/index" bytes {:fsync? false})
(bfs/write-private! (str dir "/token") token)
(bfs/atomic-write! "bin/run" script {:mode "rwxr-xr-x"})
(bfs/atomic-write! "~/.config/app/session" text {:symlink :follow :root home})
(bfs/replace-symlink! "store/app/current" "1.2.3")
```

- `briko.digest` computes SHA-256 and SHA-512 of bytes, text (UTF-8), streams and files, the last two in bounded memory, and prints digests as lowercase hex without a prefix; a format that writes `sha256-` or `sha256:` adds its own. `checksum-hex`, `checksum-hex-stream` and `checksum-hex-file` take `:md5`, `:sha1`, `:sha256` or `:sha512` for comparing a download with the checksum its publisher printed; MD5 and SHA-1 are offered only there.

```clojure
(require '[briko.digest :as digest])

(digest/sha256-hex "abc")              ;; => "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
(str "sha256-" (digest/sha256-hex-file "archive.tar.gz"))
(= published-md5 (digest/checksum-hex-file :md5 "archive.tar.gz"))
```

- `briko.inventory` reads and writes the repository inventory aro writes and krei reads: one EDN map from source id to records, sources sorted by id and records by path, one record per line, below a header of comment lines that survives a rewrite. Reading goes through `briko.edn` within 8 MiB, with no tagged literal, and refuses a record whose keys or values do not fit `record-keys`, or a source that lists one repository twice. `write!` replaces the file through `briko.fs/atomic-write!` only when its text changed; `default-path` is `aro/inventory.edn` in the XDG state home.

```clojure
(require '[briko.inventory :as inventory])

(def inv (inventory/read-file (inventory/default-path) {:missing :empty}))
(inventory/id (first (get-in inv [:sources "metio"])))   ;; => "github.com:metio/aro"
(inventory/write! (inventory/default-path) (assoc-in inv [:sources "metio"] records))  ;; => true when changed
```

- `briko.http` makes one HTTP request on `java.net.http` and answers `{:status :headers :body :url}` within a connect deadline (10 s) and a total deadline (60 s) that also covers the body and every redirect, with the body read into memory up to a limit (16 MiB) and refused, never cut short, beyond it. It requires https except to `localhost`, 127.0.0.0/8 and `[::1]`, follows no redirect unless `:max-redirects` is positive, never follows one from https to http, and drops `Authorization`, `Proxy-Authorization` and `Cookie` when a redirect leaves the origin. Header names must be HTTP tokens and values free of control characters. Every status is an answer; a refusal names the URL without its query and never carries a header value or the body. `trust-context` builds an `SSLContext` trusting only the certificates given, for a private authority.

`:as {:file path}` streams a 2xx body into a staged file beside `path` and renames it over `path` once it is complete, within `:max-bytes` (which may then exceed 2 GiB) and the deadline, forced to disk first; a body over the limit, past the deadline or cut off removes the staged file and leaves `path` untouched, and another status returns a nil `:body` without writing. A HEAD answer is not held to `:max-bytes` by its stated Content-Length. `:connect-to {:host :port}` sends an https request to another address, such as a forwarded port, through a loopback CONNECT tunnel that lives for the one request, while SNI, the Host header and certificate verification use the URL's host; a redirect leaving the origin is then refused.

`:as :lines` reads a 2xx body as UTF-8 lines while it arrives, ending at `\n` or `\r\n`, and hands each to `:on-line` on the reading thread, or collects them as `:body` without one; the answer carries `:lines-read`. A line over `:max-line-bytes` (1 MiB) is refused, or cut at a character boundary with `{:on-long-line :truncate}`; `:max-bytes` bounds the whole stream, and `:max-lines` ends it early with `{:truncated :max-lines}`. A stream still open when `:timeout-ms` ends answers with the lines read so far and `{:truncated :deadline}` instead of a refusal, the incomplete line dropped; this holds only for `:as :lines`. `:on-line` is not called after `request` returns, and a throwing `:on-line` stops the request with `::on-line-failed`, its exception left out because it may quote the line. Another status returns its body as text. `:idle-timeout-ms`, in every body mode, refuses with `::idle-timeout` a hop whose headers or next body bytes do not arrive within it, removing a staged file, so a long download can carry a generous total deadline without a stalled connection holding it.

```clojure
(require '[briko.http :as http])

(http/request :get "https://api.github.com/repos/metio/briko"
              {:headers {"Authorization" (str "Bearer " token)} :as :text :timeout-ms 20000})
;; => {:status 200 :headers {"content-type" ["application/json; charset=utf-8"] ...} :body "{...}" :url "..."}
(http/request :post "http://127.0.0.1:9090/api/v1/admin/tsdb/delete_series" {:query {"match[]" selector}})
(http/request :get url {:max-redirects 3 :allow-redirect? allowed? :max-bytes 1048576})
(http/request :get release-url {:as {:file "/tmp/tool.tar.gz" :on-progress (fn [n total] ...)}
                                :max-bytes (* 4 1024 1024 1024) :max-redirects 5 :timeout-ms 600000})
;; => {:status 200 :headers {...} :body "/tmp/tool.tar.gz" :url "..."}
(http/request :get "https://github.com/api/v3/user" {:connect-to {:host "127.0.0.1" :port 8443}})
(http/request :get watch-url {:as :lines :on-line handle-event :max-lines 20000 :timeout-ms 15000})
;; => {:status 200 :headers {...} :body nil :url "..." :lines-read 412 :truncated :deadline}
(http/request :get release-url {:as {:file dest} :timeout-ms (* 4 60 60 1000) :idle-timeout-ms 60000})
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
