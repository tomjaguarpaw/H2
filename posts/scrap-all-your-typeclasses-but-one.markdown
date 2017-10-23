# Scrap all your type classes but one

-- or, "The mother of all type classes"

In "[Scrap your type
classes](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)"
Gabriel Gonzales explains how we can replace type classes with
dictionary passing.  In this article I describe a sort of "halfway
house" to scrapping *all* our type classes.  Suppose we were only
allowed one type class.  Which would we choose?  I'll explain how we
can get (almost) all of the benefits of all type classes with only a
single type class, the "mother of all type classes" (in homage to [Dan
Piponi](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)).

We need some uncontroversial language extensions.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

and then we can present the "mother of all type classes", called
`Class`.

```haskell
class Class (f :: k -> *) (a :: k) where
  classD :: f a
```

How does this allow us to replace all type classes?  Where we
previously had

```haskell
instance MyClass a where ...
```

we will replace it with

```haskell
instance Class MyClassD a where ...
```

where `MyClassD` is the type class dictionary that [Gabriel
explained to
us](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html).
Let's implement `Show` using `Class`.  Firstly we define the
dictionary.

```haskell
newtype ShowD a = ShowD { showD :: a -> String }
```

and our instance definition fills in the missing operations, for
example for `Int` and `[Char]`.

```haskell
instance Class ShowD Int where
  classD = ShowD { showD = show }

instance Class ShowD [Char] where
  classD = ShowD { showD = show }
```

(These definitions look circular because they use `show`.  Although
I'm supposed to be showing your how to *implement* `show` using
`Class` these definitions are not circular.  I'm only using the
prexisting `show` out of laziness.  In a "real" implementation a
library author would fill in the actual body of `show`.)

Then we can define a type class polymorphic `show` function

```haskell
showC :: Class ShowD a => a -> String
showC = showD classD
```

and it works just like we would expect

```
> showC (1 :: Int)
"1"
> showC "Hello"
"\"Hello\""
```

Our familiar `Show` takes a parameter of kind `*`.  Can we do higher
kinded type parameters?  Yes, because I carefully definied `Class` to
be kind polymorphic.  Note that in its definition `a :: k`.  How
about `Functor` then?  Again, convert the `Functor` operations to a
dictionary, fill in the implementation in the instances, and define a
type class polymorphic `fmapC`.

```haskell
newtype FunctorD f =
  FunctorD { fmapD :: forall a b. (a -> b) -> f a -> f b }


instance Class FunctorD Maybe where
  classD = FunctorD { fmapD = \f x -> case x of
                         Nothing -> Nothing
                         Just x  -> Just (f x)
                    }

instance Class FunctorD [] where
  classD = FunctorD { fmapD = \f x -> case x of
                         []      -> []
                         (x:xs)  -> f x : fmapD classD f xs
                    }

fmapC :: Class FunctorD f => (a -> b) -> f a -> f b
fmapC = fmapD classD
```

It works as we expect

```
-- > fmapC (*10) (Just 1)
-- Just 10
-- > fmapC (*10) [1..4]
-- [10,20,30,40]
```

`Show` and `Functor` are single parameter type classes.  Can we do
*multi*parameter type classes?  Yes, by using a type level tuple as the
type parameter.  For example, if I want to convert the multiparameter
type class

```haskell
class Set s e where
    insert   :: s -> e -> s
    contains :: s -> e -> Bool
```

to the "mother of all type classes" setup then I can define a
dictionary with a type level tuple parameter by using a GADT.

```haskell
data SetD a where
  SetD   :: (s -> e -> s)
         -> (s -> e -> Bool)
         -> SetD '(s, e)
```

Then everything proceeds as for `Show` and `Functor`, modulo some
ceremony regarding unwrapping the GADT.

```haskell
instance Class SetD '([Int], Int) where
  classD = SetD (\s e -> e:s) (\s e -> e `elem` s)

insertC :: forall s e. Class SetD '(s, e) => s -> e -> s
insertC = case classD' of SetD insert _ -> insert
  where classD' = classD :: SetD '(s, e)

containsC :: forall s e. Class SetD '(s, e) => s -> e -> Bool
containsC = case classD' of SetD _ contains -> contains
  where classD' = classD :: SetD '(s, e)
```

```
-- > insertC [2 :: Int,3,4] (10 :: Int)
-- [10,2,3,4]
```

We have to give type annotations when we use `insertC` but that's due
to `Num` being type class polymorphic and there being no functional
dependency between `s` and `e`.  That raises a question.  Can we
encode functional dependencies in the "mother of all typeclasses"
`Class` formulation?  No, I don't see how.  I also do not see how we
can have associated types or data.

## Other approaches

Oleg Kiselyov published [a similar
idea](https://mail.haskell.org/pipermail/haskell/2007-March/019181.html)
in 2007.  I present a slight paraphrasing of Oleg's most refined
version.  It looks quite similar to the above.

```haskell
class C l t | l -> t where
    ac :: l -> t

data NUM a = NUM { nm_add :: a -> a -> a,
                   nm_mul :: a -> a -> a,
                   nm_fromInteger :: Integer -> a,
                   nm_show :: a -> String
                 }

data CLS a

instance C (CLS (NUM Int)) (NUM Int) where
    ac _ = NUM (+) (*) fromInteger show
```

Let's try and refine it further.  Firstly we notice that we're always
going to define instances of the form

```haskell
instance C (CLS (f a)) (f a) where
```

so we may as well drop the second type parameter and use

```haskell
class C t where
    ac :: CLS t -> t

instance C (f a) where
    ...
```

Then we notice that we don't need the first, phantom, argument to `ac`
any more.  Oleg only introduced it to disambiguate instances.  With
the record-based approach the type constructor of the record is
sufficient to disambiguate instances, so we can get away with merely

```haskell
class C t where
    ac :: t

instance C (f a) where
    ...
```

This is isomorphic to an existing Haskell typeclass

```haskell
class Default a where
    def :: a
```

and this approach was indeed suggested by
[`dmwit`](https://www.reddit.com/user/dmwit) in [a Reddit
comment](https://www.reddit.com/r/haskell/comments/78047z/scrap_all_your_typeclasses_but_one/doq9ldl/).
Is the `Default` approach equivalent to the `Class` approach?  Let's
consider a concrete example.  Is

```haskell
instance Default (FunctorD Maybe)
```

equivalent to

```haskell
instance Class FunctorD Maybe
```

No, because `Class FunctorD` is genuinely something of kind `(* -> *)
-> Constraint` just like `Functor` is. If I replaced the `Prelude`
definition of `Functor` with

```haskell
type Functor = Class FunctorD
```

then I would expect all of Haskell to still work the same.  There's no
way of replacing `Functor` with `Default (FunctorD Maybe)` because the
latter has an insufficiently general type.  Still, perhaps a parallel
universe Haskell ecosystem could use the latter quite happily.  What
could go wrong?  I can't think of any obvious examples but advanced
`Constraint` tricks might not be possible.

## Conclusion

You can "scrap all your type classes but one" and use the "mother of
all typeclasses instead".  I'm not suggesting you actually do this but
I do think it's very interesting.  It remains to be seen how to fit
functional dependencies and associated types and data into this
scheme.

## Acknowledgements

Thanks to [`rpglover64`](https://www.reddit.com/user/rpglover64) for
pointing out a typo.
