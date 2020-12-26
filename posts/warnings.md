# Warnings

* https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/releases

So I guess it will be released in June/July

* stm was not mirroring

> Pull mirroring failed 11 months ago.  Repository mirroring has been
> paused due to too many failed attempts, and can be resumed by a
> project maintainer.  Last successful update 11 months ago.

* I did not know that one *could* push to the GitLab mirrors.  I
  thought they were supposed to be exact duplicates.

* containers approved but did not merge
  * haddock has branches for each GHC version
  * I PRed to the wrong one
  * https://github.com/haskell/haddock/pull/1268
  * Then https://github.com/haskell/haddock/commit/acf235d607879eb9542127eb0ddb42a250b5b850
  * https://gitlab.haskell.org/ghc/ghc/-/issues/16642

* Haddock doesn't actually check that PR's build against ghc:master

https://github.com/haskell/haddock/pull/1268#issuecomment-745988796

* The only thing that will check whether I've done it right is running
  "validate" because that's the only thing that builds all the cabal
  packages with -Werror.  But by default ./validate cleans the source
  tree first and so one has to wait tens of minutes for the result

* Helpful people
  * RAE
  * Sylvain Henry
  * Alp
  * Ben Gamari
  * Sebastian Graf (check name is correct)
