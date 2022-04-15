This is my personal tagging solution.

I've developed it inch by inch as I've needed features over the past few years of working on the Cardano codebase.

I'm pushing it up on GitHub in case someone else (eg Cardano node devs) find it useful/inspiring.

It works with ghc 8.10.7 and cabal-install 3.6.2.0.
If subsequent updates break it, I'll probably fix it shortly after the Cardano project upgrades its ghc/cabal-install.

I run it in my Cardano Cabal project like this:

```
$ cabal configure
$ frisby-global-tags-cabal-ghc "$(pwd)/dist-newstyle"
$ ls G*
GPATH  GRTAGS  GTAGS
```

Cons:

- It's merely syntactic, like classic tags; no name resolution etc.

- It's comparatively very slow.

- It doesn't support all source files, eg `.hsc` files.

- It does the stupidest possible thing when associated type declarations use wildcards on their LHS.

- I'd be shocked if there are no bugs; seems to work for the Cardano codebase, at least.

- Only `cabal`, not `stack`.

- The UX is as minimal as possible; you can only run it all at once on everything in the `plan.json` that comes out of `cabal configure` (it at least skips unchanged files).

Pros:

- The code only has to pass `cabal configure` and parse; it doesn't even have to typecheck.

- I invented a scheme for tagging instance declarations.
  If you're using the Emacs mode, you can type "instance," and tab-complete to see all of the instance tags; I anticipate the pattern is easy to intuit.
