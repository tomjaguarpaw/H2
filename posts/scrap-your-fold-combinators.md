# Scrap your fold combinators

As we saw in [`foldl` traverses with `State`, `foldr` traverses with
anything](../foldl-traverses-state-foldr-traverses-anything/) every
use of `foldr` on a list is equivalent to using `traverse_` (or
`for_`) on the list with a particular choice of `Applicative`.

That implies that the fold combinators we know and love can be
replaced with uses of `for_`

The pure versions of the combinators are too convenient to avoid, but
it becomes increasingly difficult to justify combinators once they
proliferate and especially once they become effectful (because you
want the results to stream!).

### `foldl`

Don't use it

### `foldl'`

```
flip evalState z $ do
  for_ bs $ \b -> do
    a <- get
    put $! f a b
  get
```

### `foldM`

### `mapAccumL`

### `mapAccumLM`

For example from
[`GHC.Utils.Monad`](https://www.stackage.org/haddock/lts-22.14/ghc-9.6.4/GHC-Utils-Monad.html#v:mapAccumLM).


<https://discourse.haskell.org/t/break-with-traverse-traverse/9152/19?u=tomjaguarpaw>

### `mapAccumR`

Too mind-bending.  I really don't understand what this is.

### `concatMap`

### `loop`

### `loopM`

### `mapMaybe`

### `mapMaybeM`

<https://www.stackage.org/haddock/lts-22.15/extra-1.7.14/Control-Monad-Extra.html#v:mapMaybeM>

### `any`

### `anyM`
