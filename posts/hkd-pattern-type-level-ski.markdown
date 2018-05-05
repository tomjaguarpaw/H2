# The HKD pattern and type-level SKI

Let's start with a nice warm bowl of language soup.


```haskell
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Data.Functor.Identity
```

## The world before HKD

Suppose we want to parametrise a type on some type constructor.  A
prototypical exaple might be

```haskell
newtype Foo f = Foo (Maybe (f Int))
```

We can be parametric and fill in `Foo` with all sorts of goodies.  For
example, we can use different sorts of monads,

```haskell
e1 :: Foo (Writer String)
e1 = Foo $ Just $ do
  tell "Hello "
  tell "World!"
  return 2
  
e2 :: Foo IO
e2 = Foo $ Just $ do
  putStrLn "Type an Int"
  readLn
```

We can do pairs (albeit through a newtype),

```haskell
data Pair a = Pair a a

e3 :: Foo Pair
e3 = Foo (Just (Pair 1 2))
```

If we want to make make a `Reader Int String` using `Foo`'s `Int` as
the first parameter we have to do more newtype wrapping,

```haskell
newtype ReversedReader a r = ReversedReader (Reader r a)

e4 :: Foo (ReversedReader String)
e4 = Foo $ Just $ ReversedReader $ do
  i <- ask
  return (show i)
```

And if we want to store functions like `a -> f Int` we have to come up
with yet another wrapper,

```haskell
newtype ApplicativeAction a f b = ApplicativeAction (a -> f b)

e5 :: Applicative f => Foo (ApplicativeAction a f) -> a -> f (Foo Identity)
e5 (Foo Nothing) _ = pure (Foo Nothing)
e5 (Foo (Just (ApplicativeAction f))) a = fmap (Foo . Just . Identity) (f a)
```

## HKD to the rescue

This as all getting tedious so HKD comes to the rescue.  Instead of
paraemtrising an a type constructor we parametrise on some opaque
symbol (a defunctionalised version of the type constructor, if you
will).  Soo `Foo` becomes

```haskell
type family F f b

data Foo' f = Foo' (Maybe (F f Int))

data P
```

Then we no longer need a newtype for pairs,

```haskell
type instance F P a = (a, a)

e3' :: Foo' P
e3' = Foo' (Just (1, 2))
```

Or for reversed `Reader`s,

```haskell
data RR a

type instance F (RR a) r = Reader r a

e4' :: Foo' (RR String)
e4' = Foo' $ Just $ do
  i <- ask
  return (show i)
```

Or for `Applicative` actions,

```haskell
data AA a f
data Id

type instance F (AA a f) b = a -> f b
type instance F Id b = b

e5' :: Applicative f => Foo' (AA a f) -> a -> f (Foo' Id)
e5' (Foo' Nothing) _ = pure (Foo' Nothing)
e5' (Foo' (Just f)) a = fmap (Foo' . Just) (f a)
```

# Twiddling our thumbs

That's all well and good but it's still frustrating to have to define
a new symbol each time we want `Foo` to hold a different type.  What
we really want is some way of capturing the parameter to `f` (in
`Foo`'s case, `Int`) and constructing some expression with it.  Type
families and the defunctionalised symbols provide that facility but in
a clumsy and non-compositional way.  We really want a uniform and
compositional way of writing type level functions, a sort of
type-level lambda calculus, if you will.

Muse on that for a few moments while I fiddle around with a typed
version of the SKI calculus.

```haskell
data Arr a b where
  S     :: Arr a (b -> r)
        -> Arr a b
        -> Arr a r
  K     :: a -> Arr b a
  I     :: Arr a a
```

I'm going to implement the SKI calculus rules in function called `A`,
for "Algebra", or something.

```haskell
A :: Arr a b -> a -> b
A I a = a
A (K k) _ = k
A (S f x) a = (A f a) (A x a)
```

Hmm, that doesn't compile because `A` is uppercase.  I guess I *could*
make it lowercase but the uppercase version is all cool and pointy.
Maybe I'll make it a type function instead.  I'm going to use some hot
dependently type technology to lift `A` to the type level.  Luckily I
included `TypeInType` in the language soup we had for starters.

```haskell
type family A (a :: Arr k1 k2) (b :: k1) :: k2

type instance A I a = a
type instance A (K k) _ = k
type instance A (S f x) a = (A f a) (A x a)
```

Neat.  Now `A` is a great pointy letter.  Maybe it's a bit too upwards
though.  I wish it was more to the left.  The `S` is a bit boring too.
It's nice and curvy but I feel like we're missing a lowercase "a"
after all.  Let's compromise.  How about

```haskell
type (<|) f x = A f x
type (@@) = S
```

Much better!

## Abstraction at the type level

Anyway, where was I?  Back to the problem at hand.  How can we get a
uniform and compositional way of writing type level functions, like a
type-level lambda calculus?  Oh, hang on a minute!  SKI calculus is
equivalent to lambda calculus, and I just implemented it at the type
level by accident.

Right, let's make type constructor `Foo` again, this time parametrised
on something of kind `Arr * *`.

```haskell
data Foo'' f = Foo'' (Maybe (f <| Int))
```

The transformation of lambda terms into SKI terms can be somewhat
messy.  The examples we'll consider here will be within the realm of
the readable, but I wouldn't want to inflict upon you terms any more
complicated than these.  Basically, to introduce something that
doesn't depend on your type parameter use `K`, to introduce your
parameter itself use `I`, and to apply one term to another use `@@`.

For example, `\a -> (a, a)` is `K (,) @@ I @@ I`.

```haskell
e3'' :: Foo'' (K (,) @@ I @@ I)
e3'' = Foo'' (Just (1, 2))
```

Let's try another example. `\r -> Reader r String` is `K Reader @@ I @@
K String`.  Oops!  `Reader` is defined as

```haskell
type Reader r = ReaderT r Identity
```

so we can't use it without at least one argument.  Never fear,
type-level SKI is here!  We can just use `\r -> ReaderT r Identity
String` which becomes `K ReaderT @@ I @@ K Identity @@ K String`.

```haskell
e4'' :: Foo'' (K ReaderT @@ I @@ K Identity @@ K String)
e4'' = Foo'' $ Just $ do
  i <- ask
  return (show i)
```

If we want to do `\b -> a -> f b` then we need `K ((->) a) @@ (K f @@
I)`.

```haskell
e5'' :: Applicative f => Foo'' (K ((->) a) @@ (K f @@ I))
    -> a
    -> f (Foo'' I)
e5'' (Foo'' Nothing) _ = pure (Foo'' Nothing)
e5'' (Foo'' (Just f)) a = fmap (Foo'' . Just) (f a)
```

## Conclusion and discussion

We can define a single type family whose index is some combination of
a collection of compositional symbols called the SKI combinators.
Using those we can build up a type level expression equivalent to any
lambda term.

What's the downside?  You saw those terms, right?  They're unreadable.
Also, inferrence is terrible (but no worse than with defunctionalised
symbols).  Some languages (for example Scala, I believe) have native
type level lambda abstractions.  We don't have them in Haskell.  Given
the result of this investigation I assume that's purely because the
inference is bad (see also [Lennart's speculation on the
matter](https://softwareengineering.stackexchange.com/a/185439/115952)).

We could have defined SKI at the type level directly.  This would give
us composable data types as well as new type level symbols to map on.
I'm not sure that there would be a lot of point though.  It might be
worth exploring further.

```haskell
data S f g x = S (f x) (g x)
data K x y   = K x
data I x     = I x
```

Have you seen type level SKI used like this before?  [Contact
me](http://web.jaguarpaw.co.uk/~tom/contact/).
