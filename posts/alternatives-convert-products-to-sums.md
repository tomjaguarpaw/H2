# `Alternative`s convert products to sums

You may know that `Applicative`s are product-preserving functors.
That is, we have functions

    pure :: Applicative f => () -> f ()
    uncurry (liftA2 (,)) :: Applicative f => (f a, f b) -> f (a, b)

Next we will consider what sort of functors convert products to sums.
We need a function which converts an empty product to an empty sum:

    zero :: () -> f Void

(where `Void` is uninhabited).  We also need a function which converts a
pair product into a pair sum:

    pair :: (f a, f b) -> f (Either a b)

Let's simplify `zero` by removing the argument and `fmap`ping `absurd
:: Void -> a`:

    empty = fmap absurd (zero ()) :: f a

It would also be nice to simplify our interface to `pair`.  We can
get a function of a somewhat nicer type

    <|> = curry (fmap (either id id) . pair) :: f a -> f a -> f a

which can be proved equivalent to `pair` in the sense that

    pair (a, b) = fmap Left a <|> fmap Right b

But these two functions

    empty :: f a
    (<|>) :: f a -> f a -> f a

are exactly what is required for an [`Alternative`
instance](http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Applicative.html#g:2),
and so `Alternative`s are functors which convert products to sums.
