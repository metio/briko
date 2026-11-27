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
