# Space leaks in the real world

Work in progress

* `maximumBy` in `base`

  <https://gitlab.haskell.org/ghc/ghc/-/issues/10830>

* A leak in Shake

  <https://neilmitchell.blogspot.com/2013/02/chasing-space-leak-in-shake.html>

* Two leaks in Hoogle and one in Shake

  <https://neilmitchell.blogspot.com/2015/09/three-space-leaks.html>

* A simple space leak in a recursive accumulator

  <https://stackoverflow.com/questions/48034553/how-to-avoid-haskell-space-leak>

* Two leaks in Shake, one in `HashMap`

  <https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html>

* discogs2pg

  I'm not sure I really consider this a space leak. It's just bad
  design that `getTables` takes a list that it doesn't need.  On the
  other hand it's probably also bad design that `store` takes a `[a]`
  rather than some sort of stream.

  <https://clrnd.com.ar/posts/2015-08-21-profiling-a-haskell-space-leak.html>

* Space leak in GHCi

  <https://simonmar.github.io/posts/2018-06-20-Finding-fixing-space-leaks.html>


* Space leak in nested monadic loops

  Quite different from most space leaks, and really, *really* bad

  <https://ro-che.info/articles/2017-01-10-nested-loop-space-leak>

  [Discussion on GHC
  issue](https://gitlab.haskell.org/ghc/ghc/-/issues/13080)

* Space leak that should have been fixed with strict fields

  <https://sriramsami.com/haskell-optimization/>

## Taxonamy

* Edward Yang's [Space Leak Zoo](http://blog.ezyang.com/2011/05/space-leak-zoo/)

## Problems with unnecessary strictness

* A strictness slowdown in `uniplate`

  <https://neilmitchell.blogspot.com/2013/08/destroying-performance-with-strictness.html>
