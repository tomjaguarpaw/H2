# Scrap your iteration combinators

-- by Tom Ellis, April 2025

Typical programming languages have "for" and "while" loop constructs
that allow iteration over a range of numbers, over the elements of a
container, until a condition is satisfied, or simply indefinitely.
Haskell has standard library functions called `for_` (with an
underbar), `for` (without an underbar) and `forever` that work very
generally to achieve a similar purpose.  Besides those general
constructs, it has a variety of specific constructs used for looping
and iteration that could be called "iteration combinators".  This
article explains how specific iteration combinators can be replaced by
the general ones, and suggests conditions under which you might choose
to do so.

## Fold and iteration combinators

Haskell's `foldl`, `foldl'`, `foldr` and `foldM` iterate over a
container and produce a "single value" (as opposed to another
container); they are called "folds" or "fold combinators".  In the
standard library and beyond there are other functions that iterate
over a container but don't produce a "single result", instead
producing another container, and functions that iterate but not over a
container at all.  Examples of the former include `mapAccumR`,
`mapAccumL`, `mapAccumLM`, `concatMap` and `mapMaybe`.  Examples of
the latter include `loop` and `loopM`.  In general, we could call
these folds plus friends "iteration combinators".


Wow, that's a lot of iteration combinators! Is there anything we can
do to simplify dealing with this menagerie?  Well, I have frequently
been impressed by Haskell's ability to generalise seemingly disparate
concepts, and in so doing simplify them. The case of fold combinators
is no exception: they can all be rewritten in terms of `for_`; that
is, `for_` generalises every fold combinator!  The reason is that
folds over any container (or more accurately, any instance of
`Foldable`) can be written in terms of `Foldable`'s `foldr` method
but, equally, every use of `foldr` can be written in terms of `for_`
(as explained in my article "[`foldl` traverses with `State`, `foldr`
traverses with
anything](../foldl-traverses-state-foldr-traverses-anything/)").

Furthermore, the other iteration combinators can be generalised by
`for` and `forever`.  Using a smaller number of equally-powerful
concepts is generally preferable, so should we use `for_`, `for` and
`forever` in preference to specific iteration combinators?  In most
cases I would say yes.  `foldl'` is probably too simple to be worth
replacing, but in the other cases it becomes difficult to justify
specific combinators once the "loop bodies" become complicated, and
especially once they become "monadic" (the "monadic" ones are the ones
named `...M`).

Let's see how to "do equally as much with less", using `for_` to
replace fold combinators, and `for` and `forever` to replace some of
the other iteration combinators.


### `foldl`

`foldl` loops over a `Foldable` container, updating a "state
parameter" at each iteration.  It risks leaking space because it
doesn't evaluate the state parameter at each iteration; rather it
creates a new thunk.  Use `foldl'` instead to avoid the risk of space
leaks.

### `foldl'`

Here's how to replace
[`Data.List.foldl'`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#v:foldl-39-)
with `for_`.  The idea is the that "state parameter" of `foldl'`
becomes the "state parameter" of a `State` monad operation. The code
in terms of `for_` will generally be more complicated than the code
written in terms of `foldl'`, so unless the "loop body" `f` is large,
it's probably not worth using `for_` in preference.

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

(Exactly the same code actually works for any `Foldable`, not just
`[a]`, but I'll stick to lists in type signatures for simplicity.
Instead of `evalState` and a final `get` we could just use
`execState`, but I prefer to *always* use `evalState` for running
`State` monads, so I can forget about the existence of `runState` and
`execState`.)


### `foldM`

[`Control.Monad.foldM`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Monad.html#v:foldM)
can be replaced with `for_` using exactly the same code as we used for
`foldl'`, except in the `StateT` monad instead of just `State`.  We
use `foldM` instead of `foldl'` when the "loop body" `f` has some
monadic effect `m`. In such cases the loop body is often complex
enough that it is worth replacing `foldM` with `for_`.

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

(Like `foldl`, `foldM` is not strict, so to avoid a space leak one
might want to evaluate `s'` before `put`ting it.)

### `mapAccumL`

[`Data.List.mapAccumL`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#v:mapAccumL)
updates a "state parameter" at each iteration through elements of a
list, like `foldl` and `foldl'`. Additionally, it returns a list of
the same length as the input list, where each output element can
depend on the input element and the current state.  Thus we can
replace it with `for` and a `State` monad. I think the replacement is
*always* preferable to `mapAccumL`.  I am baffled by `mapAccumL` every
time I see it used but the version in terms of `for` is clear and
direct.

```.hs
mapAccumL ::
  (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumL f s0 as =
  swap $ flip runState s0 $ do
    for as $ \a -> do
      s <- get
      let (s', b) = f s a
      put s'
      pure b
```

(This implementation generalises to any `Traversable`.  In a real use
case the `swap` would probably be absorbed into the surrounding code.
Again, for strictness, we might want to evaluate `s'`.)

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
start.  For example:

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
don't particularly see the point of `mapAccumL`, so I suggest
rewriting it in terms of `reverse` and `mapAccumL`, and from there in
terms of `for` and `State`.


### `mapAccumLM`

As `foldM` generalises `foldl` to a monadic setting,
[`GHC.Utils.Monad.mapAccumLM`](https://www.stackage.org/haddock/lts-22.14/ghc-9.6.4/GHC-Utils-Monad.html#v:mapAccumLM)
generalises `mapAccumL`.  We can replace it with `for` and the
`StateT` monad.  I can't see any reason to ever use `mapAccumLM` in
practice over the version in terms of `for`.

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

(Again, in practice the `swap` will probably be absorbed into the
surrounding code, and we might want to evaluate `s'`.  See also [a
related Discourse
thread](https://discourse.haskell.org/t/break-with-traverse-traverse/9152/19?u=tomjaguarpaw).)

### `loop`

[`Extra.loop`](https://hackage.haskell.org/package/extra-1.7.14/docs/Extra.html#v:loop)
repeatedly updates a state parameter until the "loop body" signals
that it's time to break (by returning a `Left`).  That is, we are
updating a state parameter "forever", until an early return is
requested. That means we can replace `loop` with `forever` and an
`EitherT` monad which encodes the "early return" effect.  `loop` is
sufficiently simple that it's probably not worth doing this
transformation in practice.

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

(Again, you might want to evaluate `s'`.)

### `loopM`

[`Extra.loopM`](https://hackage.haskell.org/package/extra-1.7.14/docs/Extra.html#v:loopM)
is the same as `loop` except that the loop body can run in the monad
`m`, so we adjust the transformation accordingly.  In cases where the
loop bodies are complex it starts to look appealing to replace
`loopM` in this way.

```.hs
loopM ::
  (Monad m) => (s -> m (Either r s)) -> s -> m r
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

(You might find it interesting to note that the return type of the
loop body, `m (Either r s)` is isomorphic to the type `EitherT r m s`.
Ditto `s'` evaluation.)

### `concatMap`

`concatMap` iterates over a list and produces, for each element,
another list of elements.  All elements produced in this way are
concatenated into a result list.  This process is equivalent to two
nested `for_` loops, and in order to express it as such we need
something we haven't seen yet in this article: a streaming
abstraction.  Here is an implementation using the `streaming` library:

```.hs
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f as =
  toList $
    for_ as $ \a ->
      for_ (f a) $ \b ->
        yield b
```

I usually prefer to read the nested `for_` loops than a `concatMap`
and I also usually find it easier to *write* the nested `for_` loops
rather than wonder how to express my intent as a `concatMap`.  I would
always prefer to replace *nested* `concatMaps` with nested `for_`s.
In many cases, once you have adopted the streaming abstraction, you
won't actually want to use `toList`.  You can continue using the
streaming abstraction in the surrounding code.

### `mapMaybe`

`mapMaybe` serves a similar purpose to `concatMap`. Its replacement in
terms of `for_` is identical, because `for_` is polymorphic.  In the
`concatMap` replacement `for_ (f a)` was over a list and in the
`mapMaybe` replacement `for_ (f a)` is over a `Maybe`.

```.hs
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f as =
  toList $
    for_ as $ \a -> do
      for_ (f a) $ \b ->
        yield b
```

### `mapMaybeM`

<https://www.stackage.org/haddock/lts-22.15/extra-1.7.14/Control-Monad-Extra.html#v:mapMaybeM>

### `any`

### `anyM`

## `lift`ing

You'll notice that the monadic implementations are full of `lift`s.
Depending on the context that might be fine (especially if using the
`mtl` versions of operations, where sometimes the `lift`s can be
inferred, rather than the `transformers` versions) but sometimes it
might be very tedious.  In any case, as the maintainer of the Bluefin
effect system I recommend using it instead of `mtl` or `transformers`
for a `lift`-free experience.
