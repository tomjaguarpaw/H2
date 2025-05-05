# Scrap your iteration combinators

-- by Tom Ellis, May 2025

Typical programming languages have "for" and "while" loop constructs
that allow iteration over a range of numbers, over the elements of a
container, until a condition is satisfied, or simply indefinitely.
Haskell has standard library functions called
[`for_`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#v:for_)
(with an underbar),
[`for`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Traversable.html#v:for)
(without an underbar) and
[`forever`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad.html#v:forever)
that work very generally to achieve similar purposes.  Besides these
general constructs, there are a variety of specific constructs used
for looping and iteration. Taken together we might call them "iteration
combinators".  This article explains how specific iteration
combinators can be replaced by the general ones, and suggests
conditions under which you might choose to do so.

## Fold and iteration combinators

Haskell's
[`foldl`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#v:foldl),
[`foldl'`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#v:foldl-39-),
[`foldr`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html#v:foldr)
and
[`foldM`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad.html#v:foldM)
iterate over a container and produce a "single value" (as opposed to
another container); they are called "folds" or "fold combinators".  In
the standard library and beyond there are other functions that iterate
over a container but don't produce a "single result" instead producing
another container, and functions that iterate but not over a container
at all.  Examples of the former include
[`mapAccumL`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:mapAccumL),
[`mapAccumR`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:mapAccumR),
[`mapAccumLM`](https://hackage.haskell.org/package/ghc-9.12.1/docs/GHC-Utils-Monad.html#v:mapAccumLM),
[`concatMap`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:concatMap)
and
[`mapMaybe`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html#v:mapMaybe);
examples of the latter include
[`loop`](https://hackage.haskell.org/package/extra-1.8/docs/Control-Monad-Extra.html#v:loop)
and
[`loopM`](https://hackage.haskell.org/package/extra-1.8/docs/Control-Monad-Extra.html#v:loopM).
In general, we could call these folds plus friends "iteration
combinators".


Wow, that's a lot of iteration combinators! Is there anything we can
do to simplify dealing with this menagerie?  Well, I have frequently
been impressed by Haskell's ability to generalise seemingly disparate
concepts and, in so doing, simplify them. The case of fold combinators
is no exception: they can all be rewritten in terms of `for_`; that
is, `for_` generalises every fold combinator!  The reason is that
folds over any container (or more accurately, any instance of
[`Foldable`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#t:Foldable))
can be written in terms of `Foldable`'s
[`foldr`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:foldr)
method but, equally, every use of `foldr` can be written in terms of
`for_` (as explained in my article "[`foldl` traverses with `State`,
`foldr` traverses with
anything](../foldl-traverses-state-foldr-traverses-anything/)").

Furthermore, the other iteration combinators can be generalised by
`for` and `forever`.  Using a smaller number of equally-powerful
concepts is generally preferable, so should we use `for_`, `for` and
`forever` in preference to specific iteration combinators?  In most
cases I would say yes.  `foldl'` is probably too simple to be worth
replacing, but in the other cases it becomes difficult to justify
specific combinators once the "loop bodies" become complicated, and
especially once the combinators become "monadic" (the "monadic" ones
are the ones whose names end with `M`).

Let's see how to do "the same with less", using `for_` to replace fold
combinators, and `for` and `forever` to replace some of the other
iteration combinators.


### `foldl`

`foldl` loops over a `Foldable` container, updating a "state
parameter" at each iteration.  It risks leaking space because it
doesn't evaluate the state parameter at each iteration; rather it
creates a new thunk.  Use `foldl'` instead to avoid the risk of space
leaks.

### `foldl'`

Here's how to replace
[`Data.List.foldl'`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#v:foldl-39-)
with `for_`.  The idea is that the "state parameter" of `foldl'`
becomes the "state parameter" of a
[`State`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#t:State)
monad operation. The code in terms of `for_` will generally be more
complicated than the code written in terms of `foldl'`, so unless the
"loop body" `f` is large, it's probably not worth using `for_` in
preference.

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

(Exactly the same code works for any `Foldable`, not just `[a]`, but
I'll stick to lists in type signatures for simplicity.  Instead of
[`evalState`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#v:evalState)
and a final
[`get`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#v:get)
we could use
[`execState`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#v:execState),
but I prefer to *always* use `evalState` for running `State` monads,
so I can forget about the existence of
[`runState`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#v:runState)
and `execState`.)


<a name="foldM">

### `foldM`

[`Control.Monad.foldM`](https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Monad.html#v:foldM)
can be replaced with `for_` using the same code that we used for
`foldl'`, except in the
[`StateT`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#t:StateT)
monad instead of just `State`.  We use `foldM` instead of `foldl'`
when the "loop body" `f` has some monadic effect `m`. In such cases
the loop body is often complex enough that `get` and
[`put`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html#v:put)
can be absorbed into it, and it becomes worth replacing `foldM` with
`for_`.

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
depend on the input element and the state at the time the input
element is reached.  Thus we can replace it with `for` and a `State`
monad. I think the replacement is *always* preferable to `mapAccumL`.
I am baffled by `mapAccumL` every time I see it but the version in
terms of `for` is clear and direct.

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
case the `swap`, `get` and `put` would probably be absorbed into the
surrounding code.  Again, for strictness, we might want to evaluate
`s'`.)

### `mapAccumR`

`mapAccumR` is rather mind bending. [Its
documentation](https://www.stackage.org/haddock/lts-23.7/base-4.19.2.0/Data-Traversable.html#v:mapAccumR)
says

> The `mapAccumR` function ... applies a function to each element of a
> structure, passing an accumulating parameter from right to left

("Right" and "left" here really mean "end" and "start", but because we
write English from left to right the we call the last element of a
list the "rightmost" one and the first the "leftmost" one.)  In any
case, `mapAccumR` traverses a list from the end to the start.  For
example:

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

This means that `mapAccumR` is equivalent to
[`reverse`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html)ing
a list, applying `mapAccumL`, and then `reverse`ing the resulting
list. I don't particularly see the point of `mapAccumL`, so I suggest
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

(Again, in practice the
[`swap`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Tuple.html#v:swap),
`get` and `put` will probably be absorbed into the surrounding code,
and we might want to evaluate `s'`.  See also [a related Discourse
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


[`Data.Foldable.concatMap`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html)
iterates over a list and produces, for each element, another list of
elements.  All elements produced in this way are concatenated into a
result list.  This process is equivalent to two nested `for_` loops,
and in order to express it as such we need something we haven't seen
yet in this article: a streaming abstraction.  Here is an
implementation using the
[`streaming`](https://hackage.haskell.org/package/streaming) library:

```.hs
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f as =
  toList $
    for_ as $ \a ->
      for_ (f a) $ \b ->
        yield b

toList :: Stream (Of a) Identity r -> [a]
toList =
  Streaming.Prelude.fst'
   . runIdentity
   . Streaming.Prelude.toList
```

I usually prefer reading the nested `for_` loops to reading a
`concatMap`, and I usually find it easier to *write* the nested `for_`
loops than wonder how to express my intent as a `concatMap`.  I would
always prefer to replace *nested* `concatMaps` with nested `for_`s.
In many cases, once you have adopted the streaming abstraction, you
won't actually want to use `toList`.  You can continue using the
streaming abstraction in the surrounding code.

(An implementation using `pipes`, `conduit` or
[`Bluefin.Stream`](https://hackage-content.haskell.org/package/bluefin-0.0.15.0/docs/Bluefin-Stream.html)
would work equally well as `streaming`.)

### `mapMaybe`

[`Data.Maybe.mapMaybe`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html#v:mapMaybe)
serves a similar purpose to `concatMap`. Its replacement in terms of
`for_` is identical, because `for_` is polymorphic.  In the
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

[`Control.Monad.Extra.mapMaybeM`](<https://www.stackage.org/haddock/lts-22.15/extra-1.7.14/Control-Monad-Extra.html#v:mapMaybeM>)
is the monadic version of `mapMaybe`.  It is even more compelling to
replace `mapMaybeM` with `for_` than it is `mapMaybe`.

```.hs
mapMaybeM ::
  (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as =
  fmap Streaming.Prelude.fst' $
    Streaming.Prelude.toList $
      for_ as $ \a -> do
        fa <- lift (f a)
        for_ fa $ \b ->
          yield b
```

(In practice you'll likely want to avoid converting the `Stream` to a
list and instead consume the stream directly in the surrounding code.
This allows you to avoid materialising the whole list at once and
instead process the result in constant space.)

## `lift`ing

You'll notice that the monadic implementations are full of
[`lift`](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-Class.html#v:lift)s. Depending
on the context that might be fine (especially if using the `mtl`
versions of operations, where sometimes the `lift`s can be inferred,
rather than the `transformers` versions) but sometimes it might be
tedious.  In any case, as the maintainer of the
[Bluefin](https://hackage.haskell.org/package/bluefin) effect system I
recommend using Bluefin instead of `mtl` or `transformers` for a
`lift`-free experience.

## Real world example

Here's [`extend`, a real world function from
`cabal-install`](https://github.com/haskell/cabal/blob/12f6894cfb154b256342c57348cb754bb18d073d/cabal-install/Distribution/Solver/Modular/Validate.hs#L394-L417),
which uses `foldM`; we'll investigate how to change it to use `for_`
of a `StateT`.  What does `extend` do?  I don't know! But that's OK:
the procedure we're about to see is a mechanical refactoring that
preserves program behaviour.  In fact, I think it's easier to
understand what `extend` does *by transforming it to `for_` form
first!*  Let's see.

Here is the original code. The "`foldM` body" is `extendSingle`, which
inspects all the possible cases of one of its arguments to determine
what to return.  The other argument of `extendSingle` is `a`, the
"`foldM` state". Its type is `PPreAssignment` and its initial value is
`ppa`.  Many of the branches "error out" by returning `Left`. The
other branches return the "next state".

```.hs
extend ::
 (Extension -> Bool) ->
 (Language  -> Bool) ->
 (PkgconfigName -> PkgconfigVersionRange -> Bool) ->
 [LDep QPN] ->
 PPreAssignment ->
 Either Conflict PPreAssignment
extend extSupported langSupported pkgPresent newactives ppa =
  foldM extendSingle ppa newactives
  where
    extendSingle ::
      PPreAssignment ->
      LDep QPN ->
      Either Conflict PPreAssignment
    extendSingle a (LDep dr (Ext ext)) =
      if extSupported ext
      then Right a
      else Left
           (dependencyReasonToConflictSet dr,
            UnsupportedExtension ext)
    extendSingle a (LDep dr (Lang lang)) =
      if langSupported lang
      then Right a
      else Left
             (dependencyReasonToConflictSet dr,
              UnsupportedLanguage lang)
    extendSingle a (LDep dr (Pkg pn vr)) =
      if pkgPresent pn vr
      then Right a
      else Left
            (dependencyReasonToConflictSet dr,
             MissingPkgconfigPackage pn vr)
    extendSingle a (LDep dr (Dep dep@(PkgComponent qpn _) ci)) =
      let mergedDep =
            M.findWithDefault (MergedDepConstrained []) qpn a
      in case
        (\x -> M.insert qpn x a)
          <$> merge mergedDep (PkgDep dr dep ci) of
          Left (c, (d, d')) ->
            Left (c, ConflictingConstraints d d')
          Right x -> Right x
```

As explained [above, in the `foldM` section](#foldM), we should
proceed by introducing a `StateT` transformer around our inner monad
`m`, which in this case is `Either Conflict`.  We have to insert some
`lift`s to lift the `Either` into the `StateT`. We'll improve that
shortly, but for now, let's take stock:

<a name="original-extend"></a>

```.hs
import Control.Monad.Trans.State.Strict
  (StateT, evalStateT, get, put)

extend extSupported langSupported pkgPresent newactives ppa = do
  flip evalStateT ppa $ do
    for_ newactives extendSingle
    get
  where
    extendSingle ::
      LDep QPN ->
      StateT PPreAssignment (Either Conflict) ()
    extendSingle (LDep dr (Ext ext)) = do
      a <- get
      if extSupported ext
      then put a
      else lift $ Left
           (dependencyReasonToConflictSet dr,
            UnsupportedExtension ext)
    extendSingle (LDep dr (Lang lang)) = do
      a <- get
      if langSupported lang
      then put a
      else lift $ Left
             (dependencyReasonToConflictSet dr,
              UnsupportedLanguage lang)
    extendSingle (LDep dr (Pkg pn vr)) = do
      a <- get
      if pkgPresent pn vr
      then put a
      else lift $ Left
            (dependencyReasonToConflictSet dr,
             MissingPkgconfigPackage pn vr)
    extendSingle (LDep dr (Dep dep@(PkgComponent qpn _) ci)) = do
      a <- get
      let mergedDep =
            M.findWithDefault (MergedDepConstrained []) qpn a
      case
        (\x -> M.insert qpn x a)
          <$> merge mergedDep (PkgDep dr dep ci) of
          Left (c, (d, d')) ->
            lift $ Left (c, ConflictingConstraints d d')
          Right x -> put x
```

So far so mechanical, and it looks it.  The code is less clear than
before, not more.  But we can do better: there are plenty of places we
`get` the state `a`, only to `put` it straight back.  These cases
follow the pattern:

```.hs
do
  a <- get
  if ...
  then put a
  else lift $ Left
     ...
```

That's the same as not getting or putting the state at all:

```.hs
do
  if ...
  then pure ()
  else lift $ Left
     ...
```

And that's the same as using `unless`:

```.hs
extendSingle ::
  LDep QPN ->
  StateT PPreAssignment (Either Conflict) ()
extendSingle (LDep dr (Ext ext)) = do
  unless (extSupported ext) $
    lift $ Left
      (dependencyReasonToConflictSet dr,
       UnsupportedExtension ext)
extendSingle (LDep dr (Lang lang)) = do
  unless (langSupported lang) $
    lift $ Left
      (dependencyReasonToConflictSet dr,
       UnsupportedLanguage lang)
extendSingle (LDep dr (Pkg pn vr)) = do
  unless (pkgPresent pn vr) $
    lift $ Left
      (dependencyReasonToConflictSet dr,
       MissingPkgconfigPackage pn vr)
...
```

Before proceeding to eliminate the `lift`s I want to make an unrelated
refactoring: move the `M.insert qpn x` into the `Right` branch, like
so:

```.hs
case merge mergedDep (PkgDep dr dep ci) of
  Left (c, (d, d')) ->
    lift $ Left (c, ConflictingConstraints d d')
  Right x -> put (M.insert qpn x a)
```

It seems clearer than using `<$>`.  Next I'm going to eliminate the
`lift`s by switching to `mtl` and replacing `lift $ Left ...` with
`throwError ...`.  I'm also going to inline `extendSingle` so the loop
body really looks like a loop body.

<a name="final-extend"></a>

```.hs
import Control.Monad.State.Strict (evalStateT, get, put)
import Control.Monad.Except (throwError)

extend extSupported langSupported pkgPresent newactives ppa = do
  flip evalStateT ppa $ do
    for_ newactives $ \case
      LDep dr (Ext ext) -> do
        unless (extSupported ext) $
          throwError
            (dependencyReasonToConflictSet dr,
             UnsupportedExtension ext)
      LDep dr (Lang lang) -> do
        unless (langSupported lang) $
          throwError
            (dependencyReasonToConflictSet dr,
             UnsupportedLanguage lang)
      LDep dr (Pkg pn vr) -> do
        unless (pkgPresent pn vr) $
          throwError
            (dependencyReasonToConflictSet dr,
             MissingPkgconfigPackage pn vr)
      LDep dr (Dep dep@(PkgComponent qpn _) ci) -> do
        a <- get
        let mergedDep =
              M.findWithDefault (MergedDepConstrained []) qpn a
        case merge mergedDep (PkgDep dr dep ci) of
          Left (c, (d, d')) ->
            throwError (c, ConflictingConstraints d d')
          Right x -> put (M.insert qpn x a)

    get
```

I find this code very clear!  We start with an initial state of `ppa`.
For each of the `newactives`, if it is an extension, language or
package we check whether it is supported, and if not then we
`throwError`.  If it is a dependency then we check a mergeability
condition, and if it fails then we `throwError`.  If the condition
succeeds then we insert a new key-value pair into the state.  Simple.

(The `get` is a bit sad and awkward all down at the bottom on its own.
If you really don't like it you can use `execState` instead of
`evalState`.)

## Performance

If using `transformers` or `mtl` then it is likely that the
transformations described in this article will have no performance
impact, because after inlining the original and replacement versions
will optimise to the same compilation result.  If you use Bluefin (or
similarly, [effectful](https://hackage.haskell.org/package/effectful))
then it's likely you'll experience some slow down relative to the
original versions when the loop bodies are small and can be completely
inlined.  On the other hand it's possible that you will experience a
performance boost for large loop bodies that cannot be entirely
inlined (for example those that span module boundaries).

## Commentary and conclusion

*"The [final version of `extend`](#final-extend) looks imperative"* --
Yes! In fact I would say it *is* imperative. -- *"But then isn't it
the same as if it had been written in Python or Java?"*  -- No!  The
final version of `extend` is *the same* as the [original
version](#original-extend), not just in the sense that it calculates
the same result, nor even just in the sense that it calculates the
same result in the same way, but that it is a transformation of
_exactly the same code_.  This implies all the same benefits we expect
from pure functional code when it comes to maintenance and
refactoring.  For example, if `extend` had been written in Python or
Java then the type system wouldn't catch it if I slipped in a call to
delete files from disk, make network connections or launch the
missiles; in the "imperative" `extend` written in Haskell it would: I
can *only* do "`State` effects on a `PPreAssignment`", and "`Either`
effects on a `Conflict`".  This is why Haskell is "[the world's finest
imperative programming
language](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https://research.microsoft.com/~simonpj/papers/marktoberdorf/mark.pdf&type=exact)".

To sum up, instead of remembering how to use a plethora of
increasingly complicated iteration combinators, we can instead
remember how a small collection of monads or monad transformers work
(especially monads for state, exceptions and streaming), and stick to
the simple and obvious iteration combinators `for_`, `for` and
`forever`.
