# Scrap your fold combinators

-- by Tom Ellis, February 2025

In my article "[`foldl` traverses with `State`, `foldr` traverses with
anything](../foldl-traverses-state-foldr-traverses-anything/)", I
showed that every use of `foldr` on a list is equivalent to using
`traverse_` (or `for_`) on the list, with a particular choice of
`Applicative`.  Haskell libraries contain a wide variety of list
"recursion combinators" that 

That implies that the fold combinators we know and love can be
replaced with uses of `for_`

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

Too mind-bending.  I really don't understand what this is.

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
