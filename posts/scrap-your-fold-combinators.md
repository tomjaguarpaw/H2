# Scrap your fold combinators

-- by Tom Ellis, February 2025

Typical programming languages have a "for loop" construct that allows
iteration over a range of numbers or over the elements of a container.
Haskell has standard library functions called `for` and `for_` that
can be used for a similar purpose, and also has constructs `foldl`,
`foldl'`, `foldr`, `foldM`, `mapAccumR`, `mapAccumL` that iterate over
a container and produce a "single value" (as opposed to another
container); they are called "folds" or "fold combinators".  Other
folds exist beyond the standard library, including `loop`, `loopM` and
`mapAccumLM`.  Additionally there are functions that iterate over a
container but don't produce a "single result", instead producing
another container. Examples include `concatMap` and `mapMaybe`.

Wow, that's a lot of fold combinators (plus friends)! Is there
anything we can do to simplify dealing with this menagerie?  Well, I
have frequently been impressed by Haskell's ability to generalise
seemingly disparate concepts, and in so doing simplify them. The case
of fold combinators is no exception: they can all be rewritten in
terms of `for_`!

The reason for this is that folds over every container (or rather,
every instance of `Foldable`) can be written in terms of `Foldable`'s
`foldr` method but, equally, every use of `foldr` can be rewritten to
in terms of `for_` (as explained in my article "[`foldl` traverses
with `State`, `foldr` traverses with
anything](../foldl-traverses-state-foldr-traverses-anything/)").

The pure versions of the combinators are too convenient to avoid, but
it becomes increasingly difficult to justify combinators once they
proliferate and especially once they become effectful (because you
want the results to stream!).

### `foldl`

Don't use it

### `foldl'`

[`Data.List.foldl'`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#v:foldl-39-)

```.hs
foldl' :: (s -> a -> s) -> s -> [a] -> s
foldl' f s0 as =
  flip evalState s0 $ do
    for_ as $ \a -> do
      s <- get
      let s' = f s a
      put $! s'
    get
```

### `foldM`

[`Control.Monad.foldM`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Monad.html#v:foldM)

```.hs
foldM :: Monad m => (s -> a -> m s) -> s -> [a] -> m s
foldM f s0 as =
  flip evalStateT s0 $ do
    for_ as $ \a -> do
      s <- get
      s' <- lift (f s a)
      put s'
    get
```

### `mapAccumL`

[`Data.List.mapAccumL`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#v:mapAccumL)

```.hs
mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumL f s0 as =
  swap $ flip runState s0 $ do
    for as $ \a -> do
      s <- get
      let (s', b) = f s a
      put s'
      pure b
```

### `mapAccumR`

`mapAccumR` is rather mind bending. [Its
documentation](https://www.stackage.org/haddock/lts-23.7/base-4.19.2.0/Data-Traversable.html#v:mapAccumR)
says

> The `mapAccumR` function ... applies a function to each element of a
> structure, passing an accumulating parameter from right to left

("Right" and "left" here really mean "end" and "start", but because we
write English from left to right the we ended up calling the last
element of a list the "rightmost" one and the first the "leftmost"
one.)  In any case, `mapAccumR` traverses a list from the end to the
beginning.  For example:

```.hs
mapAccumRExample :: (String, [String])
mapAccumRExample =
  mapAccumR
    (\s i -> let s' = s ++ "->" ++ show i in (s', s'))
    "start"
    [1 .. 4]
```

```
ghci> mapAccumRExample
("start->4->3->2->1",["start->4->3->2->1","start->4->3->2","start->4->3","start->4"])
```

This means that `mapAccumR` is equivalent to `reverse`ing a list,
applying `mapAccumL`, and then `reverse`ing the resulting list. I
don't particularly see the point, so I'm going to skip this
combinator.


### `mapAccumLM`

[`GHC.Utils.Monad.mapAccumLM`](https://www.stackage.org/haddock/lts-22.14/ghc-9.6.4/GHC-Utils-Monad.html#v:mapAccumLM).

```.hs
mapAccumLM ::
  (Monad m) =>
  (s -> a -> m (s, b)) ->
  s ->
  [a] ->
  m (s, [b])
mapAccumLM f s0 as =
  fmap swap $ flip runStateT s0 $ do
    for as $ \a -> do
      s <- get
      (s', b) <- lift (f s a)
      put s'
      pure b
```

<https://discourse.haskell.org/t/break-with-traverse-traverse/9152/19?u=tomjaguarpaw>

### `concatMap`



### `loop`

[`Extra.loop`](https://hackage.haskell.org/package/extra-1.7.14/docs/Extra.html#v:loop)

```.hs
loop :: (s -> Either r s) -> s -> r
loop f s0 =
  runEarlyReturn $
    flip evalStateT s0 $
      forever $ do
        s <- get
        s' <- lift (f s)
        put s'

runEarlyReturn :: Either r r -> r
runEarlyReturn = either id id
```

### `loopM`

[`Extra.loopM`](https://hackage.haskell.org/package/extra-1.7.14/docs/Extra.html#v:loopM)

```.hs
loopM :: (Monad m) => (s -> m (Either r s)) -> s -> m r
loopM f s0 =
  runEarlyReturnT $
    flip evalStateT s0 $
      forever $ do
        s <- get
        fs <- (lift . lift) (f s)
        s' <- lift (except fs)
        put s'

runEarlyReturnT :: (Monad m) => ExceptT r m r -> m r
runEarlyReturnT = fmap (either id id) . runExceptT
```


### `mapMaybe`

### `mapMaybeM`

<https://www.stackage.org/haddock/lts-22.15/extra-1.7.14/Control-Monad-Extra.html#v:mapMaybeM>

### `any`

### `anyM`
