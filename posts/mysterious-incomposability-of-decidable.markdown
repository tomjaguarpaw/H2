# The Mysterious Incomposability of Decidable

`Applicative`, `Alternative` and `Divisible` are Haskell classes that
each have nice composition properties.  There is a fourth class,
`Decideable`, that fills in the remaining corner of a square of
properties but I cannot find any nice composition property for it.

By way of introduction, Haskell has a `Functor` class that can be
`fmap`ped covariantly and a `Contravariant` class that can be
`contramap`ped contravariantly.  Subclasses of these allow pairs of
the same type to be combined.  In brief

* `Applicative`: covariant, converts products to products
* `Alternative`: covariant, [converts products to
   sums](../alternatives-convert-products-to-sums/)
* `Divisible`: contravariant, converts products to products
* `Decidable`: contravariant, converts products to sums

The following code demonstrates those properties:

```haskell
import Control.Applicative
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

applicativeProduct :: Applicative f
                   => (f a, f b)
                   -> f (a, b)
applicativeProduct (fa, fb) = (,) <$> fa <*> fb

alternativeSum :: Alternative f
               => (f a, f b)
               -> f (Either a b)
alternativeSum (fa, fb) = fmap Left fa <|> fmap Right fb

divisibleProduct :: Divisible f
                 => (f a, f b)
                 -> f (a, b)
divisibleProduct (fa, fb) = divide id fa fb

decidableProduct :: Decidable f
                 => (f a, f b)
                 -> f (Either a b)
decidableProduct (fa, fb) = chosen fa fb
```

`Applicative`s, `Alternative`s and `Divisible`s compose well.  The
first with `<$>` and `<*>` and the second and third with
`fmap`/`contramap` and a monoidal operation (`<|>` and what I define
as `<+>` respectively).

```haskell
applicativeCompose :: [[String]]
applicativeCompose = f <$> [1, 2]
                       <*> [True, False]
                       <*> ["hello", "world"]
    where f = (\a b c -> replicate a (if b then c else "False"))

alternativeCompose :: [String]
alternativeCompose = fmap show [1, 2]
                     <|> fmap reverse ["hello", "world"]


divisibleCompose :: Predicate (String, Int)
divisibleCompose = contramap ((== 5) . length . fst) predicate
                   <+> contramap ((< 6) . snd) predicate
```

However, I cannot work out any nice way to compose `Decidable`s and I
find this very mysterious.  For example, suppose I have

```haskell
instance F Decidable

a :: F A
b :: F B
c :: F C

data Foo = Bar A | Baz B | Quux C
```

How do I compose `a`, `b` and `c` into an `F Foo`?  The simplest thing
I have discovered is

```haskell
compose :: F Foo
compose = f -$- (fC -*- fB -*- fA)
  where f = \case Bar a  -> Right a
                  Baz b  -> Left (Right b)
                  Quux c -> Left (Left c)
```

with the `Decidable` operations `-$-` and `-*-` given by

```haskell
(-$-) :: Contravariant f => (a -> b) -> f b -> f a
(-$-) = contramap

(-*-) :: Decidable f => f a -> f b -> f (Either a b)
(-*-) = chosen

infixl -*-
infixr -$-
```

The explicit unpacking into an `Either` is rather unsatisfactory.
I've tried all sorts of techniques, including CPS, but nothing seems
to make `Decidable`s nicely composable.  Do you have any ideas?  If
so, [contact me](http://web.jaguarpaw.co.uk/~tom/contact/)!

## Appendix

Some extra code used in the examples:

```haskell
predicate :: Predicate Bool
predicate = Predicate id

(<+>) :: Divisible f => f a -> f a -> f a
(<+>) = divide (\x -> (x, x))
```
