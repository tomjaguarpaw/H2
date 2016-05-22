# Polymorphic recursion combinator in Haskell

-- or "polymorphic fixed point combinator"

This article explains recursion combinators and polymorphic recursion,
and deduces a polymorphic recursion combinator.

## Recursion

Haskell allows us to write recursive functions, that is functions
which refer to themselves in their definitions.  Here is a recursively
defined (and somewhat inefficient) list length function.

````haskell
-- Type can be inferred
lengthList :: [a] -> Int
lengthList xs = case xs of
[]       -> 0
(_:rest) -> 1 + lengthList rest
````

    > lengthList [1,2,3]
    3


Another approach is to factor out the recursive call and make the
recursion happen elsewhere.  Notice how the parameter `recurse`
replaces the recursive call.

````haskell
-- Type can be inferred
lengthListF :: ([a] -> Int) -> [a] -> Int
lengthListF recurse xs = case xs of
  []       -> 0
  (_:rest) -> 1 + recurse rest

-- Type can be inferred
lengthList2 :: [a] -> Int
lengthList2 = lengthListF lengthList2
````

    -- > lengthList2 [1,2,3]
    -- 3

We can abstract this pattern even further by writing a "recursion
combinator" or "fixed point combinator".

````haskell
-- Type can be inferred
fix :: (a -> a) -> a
fix f = f (fix f)
````

The recursion combinator allows us to write

````haskell
-- Type can be inferred
lengthList3 :: [a] -> Int
lengthList3 = fix lengthListF
````

    -- > lengthList3 [1,2,3]
    -- 3

As an aside, the actual definition of `fix` in the Haskell standard
library is a more efficient version, but that's not important in this
article.

````haskell
-- More efficient fix
fix :: (a -> a) -> a
fix f = let x = f x in x
````

## Polymorphic recursion

As well as recursive data structures and functions like lists and
`length`, Haskell also allows us to write *polymorphically* recursive
data structures and functions.

````haskell
data Nested a = Nil | a :< Nested [a]

infixr 5 :<

nested = 1 :< [2, 3] :< [[3, 4], [5]] :< Nil
````

`Nested` is recursively defined in terms of itself but the recursive
parameter is `[a]` not the original `a`, so this recursive data type
definition is called "polymorphic".  We can write a recursive function
to calculate the length of `Nested`s.

````haskell
-- Type cannot be inferred
lengthNested :: Nested a -> Int
lengthNested ns = case ns of
  Nil      -> 0
  _ :< nns -> 1 + lengthNested nns
````

    -- > lengthNested nested
    -- 3

This is a *polymorphically* recursive function because the type of the
argument `ns`, `Nested a`, differs from the type of the argument to
the recursive call `nns`, which is `Nested [a]`.  Nonetheless we can
play the same trick as above, replacing the recursive call with a
parameter.

````haskell
-- Type can be inferred
lengthNestedF :: (Nested [a] -> Int) -> Nested a -> Int
lengthNestedF recurse ns = case ns of
  Nil      -> 0
  _ :< nns -> 1 + recurse nns

-- Type cannot be inferred
lengthNested2 :: Nested a -> Int
lengthNested2 = lengthNestedF lengthNested2
````

    -- > lengthNested3 nested
    -- 3

But trying to define this in terms of `fix` does not work.

````haskell
lengthNested2 :: Nested a -> Int
lengthNested2 = fix lengthNestedF

-- Couldn't match type `a' with `[a]'
````

The argument to `lengthNestedF` has type `Nested [a] -> Int` and the
return value has type `Nested a -> Int`.  `fix` will not work here
because the argument and return type must be the same.  We can try to
be sneaky by providing a more general type.

````haskell
-- Requires RankNTypes

-- Type cannot be inferred
lengthNestedF2 :: (forall a. Nested a -> Int) -> Nested a -> Int
lengthNestedF2 recurse ns = case ns of
  Nil      -> 0
  _ :< nns -> 1 + recurse nns
````

This gets us closer but not close enough.

````haskell
lengthNested2 :: Nested a -> Int
lengthNested2 = fix lengthNestedF

-- Couldn't match type `forall a1. Nested a1 -> Int'
-- with `Nested a -> a'
````

What hope is there of representing polymorphic recursion with a
combinator?  The answer is that we need to generalise the type of
`fix`.

````haskell
-- Requires ScopedTypeVariables

-- Type signature must be provided.
fixPolymorphic :: forall f. ((forall a. f a) -> (forall a. f a))
               -> forall a. f a
fixPolymorphic f = let x :: f b
                       x = f x
                   in x
````

To use this with `lengthNestedF2` we also need an ad hoc data type
that does nothing except massage the type `Nested a -> Int` into a
different form.

````haskell
newtype NestedFunction a =
  NestedFunction { unNestedFunction :: Nested a -> Int }

-- Type signature is not required
lengthNested2 :: Nested a -> Int
lengthNested2 =
  unNestedFunction (fixPolymorphic (\x -> NestedFunction
                                       (lengthNestedF2
                                           (unNestedFunction x))))
````

    -- > lengthNested2 nested
    -- 3

Because `NestedFunction` is a `newtype`, `NestedFunction` and
`unNestedFunction` don't actually do anything.  We only need
`NestedFunction` to massage type parameters around.  Morally, the
definition of `lengthNested2` is just

````haskell
lengthNested2 :: Nested a -> Int
lengthNested2 = fixPolymorphic lengthNestedF2
````

And there we have it, a polymorphic recursion combinator.

## Polymorphic recursion is just recursion

It's a bit disappointing that we have *two* different combinators,
though.  Can we combine them?  Yes!  With another ad hoc data type

````haskell
newtype Forall f = Forall { unForall :: forall a. f a }
````

we can write

````haskell
fixPolymorphic2 :: forall f. ((forall a. f a) -> (forall a. f a))
                -> forall a. f a
fixPolymorphic2 f = unForall (fix (\x -> Forall (f (unForall x))))
````

As above, `Forall` and `unForall` really don't do anything, and
morally the definition of the polymorphic recursion combinator is

````haskell
fixPolymorphic2 :: forall f. ((forall a. f a) -> (forall a. f a))
                -> forall a. f a
fixPolymorphic2 f = fix f
````

That is, recursion and polymorphic recursion in Haskell are exactly
the same thing!
