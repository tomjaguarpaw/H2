# `foldl` traverses with `State`, `foldr` traverses with anything

## Whether to `foldl` or `foldr`?

Avi Press gave [an excellent
talk](https://www.youtube.com/watch?v=qw4S_6FXsp4) at *Scale By the
Bay 2023* about difficulties using Haskell at a startup.  [He mentions
that](https://www.youtube.com/watch?v=qw4S_6FXsp4&t=1377s) even
experienced Haskellers don't always know how to use fundamental parts
of the language.  In particular,

> even experienced Haskell engineers aren't always going to know
> whether to
> [`foldl`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:foldl)
> or
> [`foldr`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:foldr).

In this article I'll deduce a firm rule that allows you to make the
correct choice.  I will stick to the versions of these functions that
operate on lists; their generalization to
[`Foldable`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Foldable)
warrants a separate article.  In summary, the answer is

* use
  [`foldl'`](https://www.stackage.org/haddock/lts-22.3/base-4.18.1.0/Data-Foldable.html#v:foldl-39-),
  never use `foldl`
* prefer `foldl'` over `foldr` whenever possible
* alternatively, consider just using
  [`for_`](https://www.stackage.org/haddock/lts-22.3/base-4.18.1.0/Data-Foldable.html#v:for_)

But why?  Let's see, by examining what these functions do.

## The definitions

We'll work with the traditional definitions of `foldl` and `foldr`,
given below.  The implementations in `base` are more complicated, for
performance reasons, but the reasoning in this article applies to them too.

```.hs
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (a : as) = foldl f (f z a) as

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (a : as) = f a (foldr f z as)
```

### What's the difference?

Ostensibly, the difference between `foldl` and `foldr` is the that
former "associates a binary operation to the left" and the latter
"associates a binary operation to the right", as follows.

```.hs
foldl (**) z [x1, x2, ..., xn] ==
  (...((z ** x1) ** x2) **...) ** xn

foldr (**) z [x1, x2, ..., xn] ==
  x1 ** (x2 ** ... (xn ** z)...)
```

Those are indeed descriptions of the calculated results, but that
distinction is not particularly important. If we only care about the
result then it's easy enough to convert between `foldl` and `foldr`,
as shown below.  The important difference is *how* they calculate the
result; the conversion below does not preserve behaviour. For example,
`foldl (-) 0` calculates a sequence of subtractions in constant
space[^2]; `foldr (flip (-)) 0` uses *O(n)* space.

```.hs
foldl f z == foldr (flip f) z . reverse
foldr f z == foldl (flip f) z . reverse
```

We will apply our analysis of `foldl` to its strict counterpart,
`foldl'`, too.

## `foldl` traverses with `State`

So what is the precise difference between how the two folds calculate
their results?  Consider this: suppose we didn't have `foldl`, we only
had
[`traverse`](https://www.stackage.org/haddock/lts-22.3/base-4.18.1.0/Prelude.html#v:traverse)
for
[`State`](https://www.stackage.org/haddock/lts-22.3/transformers-0.6.1.0/Control-Monad-Trans-State-Strict.html#t:State).
Nonetheless, we could recover `foldl`!  (In fact we only need
[`traverse_`](https://www.stackage.org/haddock/lts-22.3/base-4.18.1.0/Data-Foldable.html#v:traverse_),
a weaker form of `traverse`, and in these examples we'll use `for_ =
flip traverse_` for syntactic convenience.)  The example below shows
how.  It converts a function that performs `for_` (restricted to
`State`) into a function that performs `foldl`.



```.hs
foldlFromForState ::
  (forall a b. [b] -> (b -> State a ()) -> State a ()) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromForState for_ f z bs = flip evalState z $ do
  for_ bs $ \b -> do
    a <- get
    put (f a b)
  get
```

### `foldl` and `for_` (restricted to `State`) are equivalent

And not only can we get `foldl` from `for_` (restricted to `State`),
we can get `for_` (restricted to `State`) from `foldl`.  They are
equivalent!  Importantly, they are equivalent in both result and
performance characteristics.

```.hs
forStateFromFoldl ::
  (forall a b. (a -> b -> a) -> a -> [b] -> a) ->
  forall a b.
  [b] ->
  (b -> State a ()) ->
  State a ()
forStateFromFoldl foldl bs f = do
  z <- get
  put (foldl g z bs)
  where
    g a b = execState (f b) a
```

That is to say, having `foldl` is equivalent to being able to
`traverse_` in `State`.  If you have a `foldl` in your program you may
as well have used `traverse_` or `for_` with `State` (or vice versa).

### Strictness

The same analysis works for the strict left fold, `foldl'`, in place
of lazy left fold, `foldl`.  To obtain `foldl'` from `for_` we would
have to change `foldlFromForState` to use `put $! f a b` in place of
`put (f a b)`.  `forStateFromFoldl'` would be a version of `for_`
(restricted to `State`) that forces its state after every iteration.

## `foldr` traverses with anything

How does the behaviour of `foldr` differ?  Suppose we didn't have
`foldr`, we only had `for_` (the general version).  Nonetheless, we
could recover `foldr`.  The example below shows how; it converts an
`Applicative`-polymorphic `for_` into `foldr`.

```.hs
foldrFromFor ::
  (forall b f. Applicative f => [b] -> (b -> f ()) -> f ()) ->
  forall a b.
  (b -> a -> a) ->
  a ->
  [b] ->
  a
foldrFromFor for_ f z bs =
  runEndoApplicative z $ for_ bs $ \b -> mkEndoApplicative (f b)
```

I've used the following convenient type definition and functions:

```.hs
type EndoApplicative a = Const (Endo a)

mkEndoApplicative :: (a -> a) -> EndoApplicative a ()
mkEndoApplicative = Const . Endo

runEndoApplicative :: a -> EndoApplicative a () -> a
runEndoApplicative a (Const (Endo f)) = f a
```

### `foldr` and `for_` (the general version) are equivalent

And not only can we get `foldr` from `for_`, we can get `for_` from
`foldr`.  They are equivalent.  Again, the equivalence is one not only
of result but also of performance characteristics.

```.hs
forFromFoldr ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall b f.
  Applicative f =>
  [b] ->
  (b -> f ()) ->
  f ()
forFromFoldr foldr bs f =
  foldr (\b rest -> f b *> rest) (pure ()) bs
```

That is to say, having `foldr` is equivalent to being able to
`traverse_`.  If you have a `foldr` in your program you may as well
just have used `traverse_` or `for_` with an appropriate choice of
`Applicative` (or vice versa).

## Example of the difference

What can we do with this new knowledge?  Let's look at the example of
printing all even elements of a list.  We can do so using `foldr` but
the equivalent in terms of `for_` (choosing the `Applicative` to be
`IO`) is clearer.  The "equivalent" in terms of `foldl` is wrong; it
uses *O(n)* space (and completely fails on infinite lists).

```.hs
printEvensFoldr :: [Int] -> IO ()
printEvensFoldr =
  foldr
    (\i rest -> when (even i) (print i) *> rest)
    (pure ())

printEvensFor :: [Int] -> IO ()
printEvensFor is =
  for_ is $ \i -> when (even i) (print i)

printEvensFoldl :: [Int] -> IO ()
printEvensFoldl =
  foldl
    (\rest i -> when (even i) (print i) *> rest)
    (pure ())
  . reverse
```
## Solving an old riddle

An [old riddle](https://wiki.haskell.org/Foldl_as_foldr) challenges us
to write `foldl` in terms of `foldr`. Personally I find the riddle
impossible to solve directly and even when I know the answer I can
hardly understand it.  With the code above, though, we can solve the
riddle with no further thought. We know how to turn `foldr` into
`for_` and `for_` into `foldl`, so we simply compose.

```.hs
foldlFromFoldr ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr foldr =
  foldlFromForState (forFromFoldr foldr)
```

After purely mechanical simplification (see appendix below) this
becomes

```.hs
foldr (\b rest a -> rest (f a b)) id bs z
```

We didn't need to use any brainpower to solve the riddle!  The riddle
can also be solved for `foldl'`. It ends up the same except with a
strict application:

```.hs
foldr (\b rest a -> rest $! f a b) id bs z
```

## Should I use `foldl'`, `foldl` or `foldr`?

Now we're ready to resolve the original dilemma.  We've seen that the
only difference in functionality between `foldl` and `foldr` is that
the latter can be used in a wider range of situations.  In consequence
we can specify when one should be used in preference to another.

### Never use `foldl`

Firstly, regarding the choice between the two left folds, always use
the strict version `foldl'`, not the lazy version `foldl`.  The latter
can cause space leaks when strictness analysis is off; the former
always avoids those space leaks. I've never seen a reason to use
`foldl`.[^1] This advice is common knowledge in the Haskell community
and not directly the subject of this article but it seems appropriate
to reiterate it.

### Use `foldl'` in preference to `foldr`

Regarding the choice between left and right fold, our first thought
might have been been to take into account the ostensible distinction
that the former "associates to the left" and the latter "to the
right".  But we are no longer distracted by this mirage.  We saw above
that `foldl'` is equivalent to `for_` restricted to (a strict use of)
`State`, and `foldr` is equivalent to general `for_`, so `foldl'` is a
special case of `foldr`. According to the [Principle of Least
Power](https://wiki.c2.com/?PrincipleOfLeastPower) you should use
`foldl'` in preference to `foldr` when you can.

### Maybe just use `for_`

`foldr` does not do a *different* job to `foldl'`: it does a more
general version of the *same* job.  This was not immediately clear,
however; it required careful analysis.  The lack of clarity around the
behavior of folds might be an argument for avoiding them and instead
using `for_` with an appropriate choice of `Applicative`.  Personally,
I find `for_` much clearer than `foldr` in many cases.  The `base`
[implementation](https://hackage.haskell.org/package/base-4.19.0.0/docs/src/GHC.List.html#%21%3F)
of `(!?)`, below, is a case in point.  It is a `foldr` that simulates
a `for_` in a composition of `StateT` and `Either` by handwriting the
bind (`(>>=)`).

```.hs
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k ->
      case k of
        0 -> Just x
        _ -> r (k-1)) (const Nothing) xs n
```

I find it much clearer written as a literal `for_`, as below (but it
can't be, because `StateT` isn't in `base`).  The two implementations
should have equal performance when compiled, assuming sufficient
inlining, because `for_` for lists in `base` is implemented in terms
of `foldr`.

```.hs
xs !? n =
  | n < 0 = Nothing
  | otherwise =
      fromEither $ do
        flip evalStateT n $ do
          for_ xs $ \x -> do
            get >>= \case
              0 -> lift (Left (Just x))
              k -> put (k - 1)
        Left Nothing

fromEither :: Either a a -> a
fromEither = either id id
```

### Isn't `for_` imperative?

Yes.  Although I don't know how to precisely define "imperative style"
I am confident in saying that the style of `(!?)` defined with `foldr`
is functional and the style of `(!?)` defined with `for_` is
imperative. Yet they calculate exactly the same thing in exactly the
same way. In fact, I think the implementation with `for_` is both
imperative and clearer.  How can that be?  Don't functional
programmers eschew imperative style?  Actually, no: I also think that
Haskell is the world's finest imperative programming language!

What does the algorithm look like in other imperative programming
languages?  Below are two implementations in Python.  Each shows the
weakness of Python's support for imperative programming compared to
Haskell.  In the first example the scopes of `k`, `ret` and `x` are
limited to no less than the rest of the function, and the `break` is
implicitly scoped to the closest enclosing `for` -- you don't get a
choice about that.  By contrast, the corresponding Haskell variables
are scoped to their precise range of use, and the `break` equivalent
is scoped precisely to its enclosing handler, `fromEither`. The scope
is maintained even across function call boundaries so you can refactor
and abstract.  The second Python example is no better; it just trades
a too-large scope of `ret` for an abstraction-resistant scope of early
`return`.

I find imperative style programming in Haskell clear to understand and
easy to reason about exactly because Haskell's type system and
expression-based nature allows fine-grained effect tracking and
precise control of the scopes of values and effects.

Python example avoiding early return:


```.py
def lookup(xs, n):
  if n < 0: return None
  k = n
  ret = None
  for x in xs:
    if k == 0:
      ret = x
      break
    k -= 1

  return ret
```

Python example with early return:

```.py
def lookup(xs, n):
  if n < 0: return None
  k = n
  for x in xs:
    if k == 0:
      return x
    k -= 1

  return None
```

## Conclusion

By carefully analysing the behaviour of three Haskell folds on lists
we were able to determine when we should use each.  We even discovered
that "imperative style" programming in Haskell can be clearer than
"functional style". In summary, never use `foldl`, prefer `foldl'` to
`foldr` where you can, or maybe just forget them all and use `for_`
instead.

## References and commentary

### `Monoid`s

This article is the culmination of an idea I wrote about years ago:
*[What is `foldr` made
of?](http://web.jaguarpaw.co.uk/~tom/blog/posts/2012-11-04-what-is-foldr-made-of.html)*.
Brent Yorgey wrote a follow-up, *[`foldr` is made of
monoids](https://byorgey.wordpress.com/2012/11/05/foldr-is-made-of-monoids/)*.
One lesson of the present article is that `foldr` being "made of
monoids" is equivalent to it being "made of applicatives whose result
type we ignore", because when you have an applicative whose result
type you ignore, that's equivalent to having a monoid.

There is also a *specific* `Applicative` that corresponds directly to
any given `Monoid` `m`, that is, `Const (Endo m)`; that's why we used
`Const` and `Endo` to define `foldrFromFor`.  Instead of "`foldr`
traverses with anything" we could have said "`foldr` traverses with
`Const (Endo m)`" (but that's much less catchy).  However, it is worth
observing that the following characterizations are also valid:

* "`foldl` `foldMap`s with `Dual (Endo _)`"
* "`foldr` `foldMap`s with any `Monoid`"
* "`foldl'` `foldMap`s with `StrictEndo _`"
* "`foldr` `foldMap`s with `Endo _`"

(where `StrictEndo` is [a strict version of `Endo`, which doesn't seem
to exist anywhere in the Haskell
ecosystem](https://discourse.haskell.org/t/is-there-a-strict-endo/8441)).
The lens library takes advantage of these correspondences in its
definitions of
[`foldlOf'`](https://www.stackage.org/haddock/lts-22.4/lens-5.2.3/Control-Lens-Fold.html#v:foldlOf%27),
[`foldlOf`](https://www.stackage.org/haddock/lts-22.4/lens-5.2.3/Control-Lens-Fold.html#v:foldlOf)
and
[`foldrOf`](https://www.stackage.org/haddock/lts-22.4/lens-5.2.3/Control-Lens-Fold.html#v:foldrOf)
(although it uses `Endo (Endo _)` instead of `StrictEndo _`).

In any case, the slogan "`foldl` traverses with `State`, `foldr`
traverses with anything" seems the most catchy and the easiest to use
as a guide to practice.

### `foldM`

There is corresponding characterization of `foldM`: "`foldM` traverses
with `StateT`", as demonstrated by the following, which is a minor
adjustment to the `foldl` equivalents.

```.hs
foldMFromForStateT ::
  ( forall a b m.
    Monad m =>
    [b] ->
    (b -> StateT a m ()) ->
    StateT a m ()
  ) ->
  forall a b m.
  Monad m =>
  (a -> b -> m a) ->
  a ->
  [b] ->
  m a
foldMFromForStateT for_ f z bs = flip evalStateT z $ do
  for_ bs $ \b -> do
    a <- get
    put =<< lift (f a b)
  get

forStateTFromFoldM ::
  ( forall a b m.
    Monad m =>
    (a -> b -> m a) ->
    a ->
    [b] ->
    m a
  ) ->
  forall a b m.
  Monad m =>
  [b] ->
  (b -> StateT a m ()) ->
  StateT a m ()
forStateTFromFoldM foldM bs f = do
  z <- get
  put =<< lift (foldM g z bs)
  where
    g a b = execStateT (f b) a
```

### "It's always `traverse`"

There's a running joke in the Haskell world that "it's always
`traverse`", that is, many complicated transformations can be boiled
down to a use of `traverse`.  This article sheds more light on that
phenomenon; `foldl`, `foldl'`, `foldr` and `foldM` are just different
flavours of `traverse_`.

### `Foldable`

This article only discusses the relationship between `foldl` and
`foldr` as functions on lists, not as functions in the `Foldable`
class; that can of worms deserves an article of its own.  However, the
slogan "`foldl` traverses with `State`, `foldr` traverses with
anything" can be seen as specifying a putative *law* for how the
`Foldable` versions of these functions should behave.  Determining
whether that law holds in practice, and role of the mysterious
`foldr'`, requires further analysis.

## Appendix: Calculations for solving the riddle

He we do the calculation for `foldl`.  The calculation for `foldl'` is
almost identical.  Starting from the original definition of
`foldlFromFoldr`, after inlining `foldlFromForState` and
`forFromFoldr`, we get

```.hs
foldlFromFoldr ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr foldr f' z bs' =
  flip evalState z $ do
    for_ bs' $ \b -> do
      a <- get
      put (f' a b)
    get
  where
    for_ bs f = foldr (\b rest -> f b *> rest) (pure ()) bs
```

Then we can extract the body of `for_` as a variable `g`

```.hs
flip evalState z $ do
  for_ bs' g
  get
where
  for_ bs f = foldr (\b rest -> f b *> rest) (pure ()) bs
  g b = do
    a <- get
    put (f' a b)
```

Then inline `for_`

```.hs
flip evalState z $ do
  foldr (\b rest -> g b *> rest) (pure ()) bs
  get
where
  g b = do
    a <- get
    put (f a b)
```

Write `g` in terms of `modify`

```.hs
flip evalState z $ do
  foldr (\b rest -> g b *> rest) (pure ()) bs
  get
where
  g b = modify (flip f b)
```

and then inline `g`

```.hs
flip evalState z $ do
  foldr (\b rest -> modify (flip f b) *> rest) (pure ()) bs
  get
```

and use `execState` rather than `evalState` with `get`

```.hs
flip execState z $ do
  foldr (\b rest -> modify (flip f b) *> rest) (pure ()) bs
```

Now we can take advantage of an interesting property of `foldr`, that
when `h . g == id` we have the equality

```.hs
foldr (\a -> f a) z == h . foldr (\a -> g . f a . h) (g z)
```

(I don't know if this is a free theorem or whether you have to prove
it by induction.)  Observing that `execState` and `modify` are
inverses we get

```.hs
flip execState z $ do
  modify $
    foldr
      (\b rest -> execState $ modify (flip f b) *> modify rest)
      (execState (pure ()))
      bs
```

We can combine the two `modify`s to get

```.hs
flip execState z $ do
  modify $
    foldr
      (\b rest -> execState $ modify (rest . flip f b))
      (execState (pure ()))
      bs
```

and use that `execState . modify == id` to get

```.hs
foldr
  (\b rest -> rest . flip f b)
  (execState (pure ()))
  bs
  z
```

which is

```.hs
foldr (\b rest a -> rest (f a b)) id bs z
```

The same calculation for `foldl'` yields

```
foldr (\b rest a -> rest $! f a b) id bs z
```


[^1]: If you've miraculously discovered a case where `foldl` is better
 then please [tell me](http://web.jaguarpaw.co.uk/~tom/contact)
 because I would be astonished to learn about it.

[^2]: assuming strictness analysis is on, otherwise it has space leak
 -- `foldl'` always calculates it in constant space
