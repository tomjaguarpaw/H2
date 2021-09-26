# Nested strict data in Haskell

## Introduction

Every so often [someone bemoans the space
leaks](https://www.reddit.com/r/haskell/comments/pvosen/how_can_haskell_programmers_tolerate_space_leaks/.compact)
that can arise due to Haskell's laziness.  A frequently touted remedy
is to make data stricter by turning on `BangPatterns`, by defining
data structures with explicitly strict fields, or by creating
implicitly strict fields with the `StrictData` extension.  Each of
these approaches leaves something to be desired.  In this article I'll
explain how the approaches work, what they leave to be desired, and a
suggest a reasonably general alternative.  The alternative seems
lightweight enough for Haskell programmers to adopt when they define
strict data structures.

## The problem

Consider the function `pairFoldBad`:

```haskell
pairFoldBad :: (Integer, Integer)
pairFoldBad = foldl' f (0, 0) [1..million]
  where f (count, theSum) x = (count + 1, theSum + x)
```

Strict `foldl'` was the correct thing to use here, rather than lazy
`foldl`, so why is the function bad?  Because each pair component
(`count` and `theSum`) is not necessarily an evaluated `Integer`,
merely a *thunk which can be evaluated to an `Integer`*.  Each time
`f` processes a list element `x` the thunk `count` has a `+ 1`
operation added on top of it and the thunk `theSum` has a `+ x`
operation added on top of it.  After `foldl'` has finished, the return
value of `pairFoldBad` will be a pair of two thunks, each one million
elements deep!  In other words, it has a space leak.

## Solution with bang patterns

A typical solution is to use bang patterns to make sure `count` and
`theSum` are evaluated on the way in to `f`, as in `pairFoldBangs`.

```haskell
pairFoldBangs :: (Integer, Integer)
pairFoldBangs = foldl' f (0, 0) [1..million]
  where f (!count, !theSum) x = (count + 1, theSum + x)
```

Each time around the loop `f` returns two thunks of depth 1. The
subsequent call to `f` takes them as arguments. The bang patterns
(i.e. the `!` symbols) evaluate each of the thunks to evaluated
`Integer`s.  The overall return value of the `foldl'` is a pair of
depth 1 thunks.

This does the job.  It's a little bit weird that `f` produces thunks
of depth 1 because that means the `foldl'` produces thunks of depth 1
and we really want evaluated `Integer`s.  They are evaluated
immediately to `Integers` as soon as we use them and there's no space
leak but it feels like we're doing something not exactly right.

An alternative that produces a pair of evaluated `Integer`s is
`pairFoldBangsAwkward`.  It ensures that the pair components are
evaluated on the way *out* (i.e. when the pair is created) not on the way
*in* (i.e. when the pair is inspected).

```haskell
pairFoldBangsAwkward :: (Integer, Integer)
pairFoldBangsAwkward = foldl' f (0, 0) [1..million]
  where f (count, theSum) x = let !count'  = count + 1
                                  !theSum' = theSum + x
                              in (count', theSum')
```

This form is rather unwieldy though.  No less unwieldy is use of the
strict function application operator `$!`:

```haskell
...
  where f (count, theSum) x = ((,) $! count + 1) $! theSum + x
```

The major drawback of using `BangPatterns` to solve this problem is
that we have to remember to do so!  The type system does not guide us
to write our program correctly.  The program is type correct even if
we omit all the bang patterns.

## Solution with strict data type

To get some help from the type system we can switch out Haskell's
standard pair type for one we define ourselves, with strict fields:

```haskell
data StrictPair a b = StrictPair !a !b
```

Then when we write `pairFoldStrictPair` as below there is no space
leak.

```haskell
pairFoldStrictPair :: StrictPair Integer Integer
pairFoldStrictPair = foldl' f (StrictPair 0 0) [1..million]
  where f (StrictPair count theSum) x = StrictPair (count + 1) (theSum + x)
```

Why is there no space leak?  This code looks exactly the same as the
original problematic code `pairFoldBad`, except we are using the
`StrictPair` type we defined ourselves instead of Haskell's built-in
pair.  Why is it different?  It is different because whenever a value
is constructed using a constructor with a strict field (i.e. a field
with a `!` in front of it in the `data` declaration, such as the
fields of `StrictPair` above) [the compiler inserts code to evaluate
that
field](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-710004.2.1).
In the case of `pairFoldStrictPair` the code that is generated is the
same as if we had written the desugared form
`pairFoldStrictPair_Desugared` below.

```haskell
pairFoldStrictPair_Desugared :: StrictPair Integer Integer
pairFoldStrictPair_Desugared = foldl' f (StrictPair 0 0) [1..million]
  where f (StrictPair count theSum) x = let !count'  = count + 1
                                            !theSum' = theSum + x
                                        in StrictPair count' theSum'
```

This is helpful: we now cannot avoid being strict.  If we use the
`StrictPair` type then we can't forget to evaluate the components.

The major drawback of defining strict data types to replace the more
familiar lazy ones is that they really are completely different types
with completely different associated libraries (if any).  We can't use
the standard `fst` and `snd` functions on `StrictPair`, for example
(although [libraries do
exist](https://hackage.haskell.org/package/strict-0.4.0.1/docs/Data-Strict-Tuple.html#v:fst)
that provide this functionality).  It is necessary to explicitly
convert back and forth between `(,)` and `StrictPair`.

## A problem with strict nested fields

A further problem with strict data fields is that users often think
that they provide more benefit than the reality.  For example, from
the above we know not to write `maybeFoldBad`:

```haskell
maybeFoldBad :: (Integer, Maybe Integer)
maybeFoldBad = foldl' f (0, Nothing) [1..million]
  where f (i, Nothing) x = (i + 1, Just x)
        f (i, Just j)  x = (i + 2, Just (j + x))
```

Perhaps we should try writing it with a `StrictPair` instead:

```haskell
maybeFoldStillBad :: StrictPair Integer (Maybe Integer)
maybeFoldStillBad = foldl' f (StrictPair 0 Nothing) [1..million]
  where f (StrictPair i Nothing)  x = StrictPair (i + 1) (Just x)
        f (StrictPair i (Just j)) x = StrictPair (i + 2) (Just (j + x))
```

This is still no good!  The problem is that although the `Maybe
Integer` in the second component of the `StrictPair` is strictly
evaluated that only means that evaluating the constructor of the
`StrictPair` evaluates the *constructor* of the `Maybe`.  The payload
of the `Just` is not evaluated so we build up a layer of thunk each
time around the loop.

It is common in the Haskell world to see strict data field definitions
like

```haskell
data MyData = MyData { field1 :: !String
                     , field2 :: ![Double]
                     , field3 :: !Maybe Bool
                     }
```

Those strict fields probably don't do what the author hoped!  They are
almost entirely pointless.  The bang annotations on the `String` and
list mean that those fields are only evaluated one cons cell deep.
The tail of the data structure is left unevaluated, as is the first
element.  Similarly the `Maybe Bool` is only evaluated to a `Nothing`
or `Just`.  If it's the latter then its payload is unevaluated.

Having noted this caveat we can find a way of addressing the problem
in our case.  `maybeFoldBangs` is just too painful to write by hand,
and besides, we might leave out a bang accidentally.  Instead we can
repeat the strict data type creation process and define `StrictMaybe`
(indeed [this has already been done for
us](https://hackage.haskell.org/package/strict-0.4.0.1/docs/Data-Strict-Maybe.html#t:Maybe))
and write `maybeFoldStrictMaybe`, a function without space leaks.

```haskell
maybeFoldBangs :: (Integer, Maybe Integer)
maybeFoldBangs = foldl' f (0, Nothing) [1..million]
  where f (!i, Nothing) x = (i + 1, Just x)
        f (!i, Just !j) x = (i + 2, Just (j + x))

data StrictMaybe a = StrictNothing | StrictJust !a deriving Show

maybeFoldStrictMaybe :: StrictPair Integer (StrictMaybe Integer)
maybeFoldStrictMaybe = foldl' f (StrictPair 0 StrictNothing) [1..million]
  where f (StrictPair i StrictNothing)  x = StrictPair (i + 1) (StrictJust x)
        f (StrictPair i (StrictJust j)) x = StrictPair (i + 2) (StrictJust (j + x))
```

This works fine, but we're going down a path where we will have to
deal with two universes of data types: one lazy universe and one
strict universe.

## Unifying strict and lazy data types

Can we do better than two distinct universes?  Yes, I think we can!
Let's define a `newtype` `Strict` with which we will represent the
invariant: "when it is evaluated all its immediate children are evaluated
too".  By way of convenience we can define a typeclass `Strictly` to
allow us to create `Strict` values and a pattern synonym `Strict` to
allow us to extract values from the `newtype` (we should be careful
with the actual constructor because it should be used only in ways
which preserve the invariant).

```haskell
-- Any value of `Strict` should satisfy the invariant that when it is
-- evaluated then all its immediate children are evaluated too.
--
-- The constructor is "unsafe" in the sense that if you don't ensure
-- the invariant holds when you use it then you will violate the
-- expectations of the consumer.
newtype Strict a = MkStrictUnsafe a deriving Show
pattern Strict a = MkStrictUnsafe a

class Strictly a where
  strict :: a -> Strict a

instance Strictly (a, b) where
  -- This is a safe use of MkStrictUnsafe because it satisfies the
  -- invariant!  We know a and b are evaluated at the point we
  -- construct the pair.
  strict (!a, !b) = MkStrictUnsafe (a, b)
```

Now let's see an example of using our "`Strict` pair" to write a pair
fold.  In `pairFoldStrict` the `Strict` type guides us to write
correct, space leak free, code, which was the benefit of `StrictPair`.
On the other hand we don't have the downside of `StrictPair`: there is
no new data type.  We can interoperate freely with the existing
ecosystem!

```haskell
pairFoldStrict :: Strict (Integer, Integer)
pairFoldStrict = foldl' f (strict (0, 0)) [1..million]
  where f (Strict (count, theSum)) x = strict (count + 1, theSum + x)
```

We can also freely compose `Strict` types.  After defining a standard
`Strictly` instance for `Maybe` the fold with `Maybe` can be written,
space leak free, as `maybeFoldStrict`.

```haskell
instance Strictly (Maybe a) where
  strict = \case
    -- This is a safe use of MkStrictUnsafe because it satisfies
    -- the invariant.  Nothing has no children.  Just has one child
    -- which we know is evaluated when we construct the Strict Maybe.
    Nothing -> MkStrictUnsafe Nothing
    Just !x -> MkStrictUnsafe (Just x)

maybeFoldStrict :: Strict (Integer, Strict (Maybe Integer))
maybeFoldStrict = foldl' f (strict (0, strict Nothing)) [1..million]
  where f (Strict (i, Strict Nothing))  x = strict (i + 1, strict (Just x))
        f (Strict (i, Strict (Just j))) x = strict (i + 2, strict (Just (j + x)))

```

## What could `Strict` buy us in practice?

`Strict` could buy us the ability to conveniently define strict nested
data types without requiring a parallel universe of strict types.  We
now know that writing

```haskell
data MyData = MyData
    { field1 :: !Either Int Bool
    , field2 :: !(Maybe Double, Data.Map.Strict.Map Int Float)
```

doesn't make a data type as strict as we probably hoped.  Instead of
the parallel universe version

```haskell
data MyData = MyData
    { field1 :: !StrictEither Int Bool
    , field2 :: !StrictPair (StrictMaybe Double)
                            (Data.Map.Strict.Map Int Float)
```

we can use `Strict` with the existing universe of data types

```haskell
data MyData = MyData
    { field1 :: !Strict (Either Int Bool)
    , field2 :: !Strict (Strict (Maybe Double),
                         Data.Map.Strict.Map Int Float)
```

### Performance impact

If `strict` is inlined then the compiler ought to be able to determine
whether constructor arguments have already been evaluated and thus
avoid redundantly evaluating them again.

## What can't `Strict` buy us?

I don't see how `Strict` could help much with large lazy data
structures such as lists (including `String`s).  The only way that I
can see to use `Strict` with standard lists whilst satisfying its
invariant would be to walk the whole list, which is prohibitively
inefficient.  Instead I recommend not using large lazy data structures
anywhere one desires strictness.  Instead use strict data structures
such as strict `Text`, `ByteString`, `Map`, `Vector` or `Array` (I'm
not sure of the strictness characteristics of `Seq` and I haven't
validated the strictness guarantees of `Vector` or `Array`.  That will
have to be another article.)


### Performance impact

Unfortunately although, as observed above, inlining `strict` ought to
allow us to avoid redundant evaluations when constructing I don't
think we can avoid redundant evaluation when destructing.  For
example, if we write

```haskell
case strictMaybe of
    Strict (Just x) -> let !x' = x in f x'
    ...
```

then we would like the compiler to be able to elide the evaluation of
`x`, as below, because, given our implementation, `x` has already been
evaluated.

```haskell
case strictMaybe of
    Strict (Just x) -> f x
    ...
```

However, short of making `Strict` built-in to the compiler, I don't
see how this could be possible.  The compiler doesn't know that
someone hasn't violated the invariant of `MkStrictUnsafe`, after all!

On the other hand the compiler *could* (I don't know if GHC does)
elide the same evaluation if the code used `StrictMaybe`.  It knows
that the payload of a `StrictJust` is always evaluated because it
itself ensures that when each `StrictJust` is constructed!

```haskell
case strictMaybe of
    StrictJust x -> let !x' = x in f x'
    ...

-- can be rewritten to

case strictMaybe of
    StrictJust x -> f x
    ...
```

For this reason, destructing `Strict` values is probably going to be
less efficient than destructing values of individually handwritten
types from a strict universe.  It's probably not a big deal for the
vast majority of code though.

## Conclusion

Defining strict fields that contain lazy types is almost completely
pointless:

```haskell
data MyData = MyData { field1 :: !String
                     , field2 :: ![Double]
                     , field3 :: !Maybe Bool
                     }
```

As an alternative, it is an open question whether `Strict`, as defined
in this article, can prove general enough to achieve widespread use or
whether the solution is a parallel universe of strict data types.

Have you seen or used anything like `Strict` before?  If so please
[contact me](http://web.jaguarpaw.co.uk/~tom/contact).
