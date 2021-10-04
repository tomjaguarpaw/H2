# Haskell's `Data.Map.Strict.Map` is not a strict map

Summary: *I was surprised when I learned that `Data.Map.Strict.Map` is
not strict.  Its laziness has serious consequences for attempts at
space leak free programming in Haskell.*

The Haskell
[`containers`](https://hackage.haskell.org/package/containers) package
has a module called
[`Data.Map`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map.html)
which provides an associative container type.  That module just
re-exports
[`Data.Map.Lazy`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Lazy.html)
so it is a *lazy* associative container type.  What does the word
"lazy" mean here?  Have a look at this example:

```haskell
> import qualified Data.Map.Lazy as L
>
> L.delete 0 $ L.insert 0 undefined $ L.empty
fromList []
```

We started with an empty map, inserted the entry `undefined` under the
key `0`, and then deleted the entry under key `0`.  The result was an
empty map.  We call this data structure "lazy" because its operations
do not evaluate the entries it contains.  Instead the entries are left
as unevaluated thunks until some consumer evaluates them.  (The tree
structure of the map and the keys of the map are not lazy, however.)
We know the map does not evaluate its entries because if it did we
would have seen an exception when it came to evaluate `undefined`.

Contrast this lazy behaviour with the behaviour of `Data.Map.Strict`:

```haskell
> import qualified Data.Map.Strict as S
>
> S.delete 0 $ S.insert 0 undefined $ S.empty
fromList *** Exception: Prelude.undefined
...
```

The strict map ensures that the entries are always kept evaluated
(when the map itself is).  We see that it tried to evaluate
`undefined` and thus raised an exception.

The [strict version is useful for avoiding space
leaks](../nested-strict-data/) when making many adjustments to a map.
It avoids the entries becoming unevaluated thunks, growing in size.
For example, this function has a space leak:

```haskell
countParity :: [Int] -> L.Map String Integer
countParity = foldl' inc (L.fromList [("even", 0), ("odd", 0)])
    where inc m n = if n `mod` 2 == 0
                    then L.adjust (+1) "even" m
                    else L.adjust (+1) "odd" m
```

and this function doesn't:

```haskell
countParityStrict :: [Integer] -> S.Map String Integer
countParityStrict = foldl' inc (S.fromList [("even", 0), ("odd", 0)])
    where inc m n = if n `mod` 2 == 0
                    then S.adjust (+1) "even" m
                    else S.adjust (+1) "odd" m
```

If we want to make a larger data type that contains an associative map
and that has many modifications performed to it over the lifetime of a
program should we should use a strict map like this

```haskell
data MyData = MyData { myBool :: !Bool
                     , myMap  :: !(S.Map String Integer)
                     }
```

rather than a lazy map like this?

```haskell
data MyData = MyData { myBool :: !Bool
                     , myMap  :: !(L.Map String Integer)
                     }
```

It's a trick question!  Suppose I diligently chose the first option
but my colleague who was using my data type comes to me and says "I
have a space leak when using `MyData`".  How could that be?!

`Data.Map.Strict.Map` *is not a strict map*.  It is the *same type* as
`Data.Map.Lazy.Map`!  We saw strict behaviour above because
`Data.Map.Strict` is a *strict interface to a lazy map*.  It does not
itself provide a strict map. [The
documentation](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html)
is explicit about this

> Each function in this module is careful to force values before
> installing them in a Map. ... If all values stored in all maps in
> the arguments are in WHNF, then all values stored in all maps in the
> results will be in WHNF once those maps are evaluated.

Perhaps it could afford to be *more* explicit that the type
`Data.Map.Strict.Map` is exactly the same as its lazy counterpart, but
it's not trying to hide anything.  Nonetheless it was closer to a
decade than a year into my Haskell journey that I learned this.

Consequently, even if you diligently attempt to avoid space leaks by
defining strict data types then elsewhere in your program you or your
colleague can innocently subvert your best intentions by using the
lazy map interface rather than the strict one.  Both interfaces work
on the same map type so the type system does not help here.  It
compiled but it did not work!

What are the consequences for attempts space leak free programming in
Haskell?  Unfortunately you cannot counteract space leaks in
`Data.Map` *locally*, by ensuring that you use it in data types only
as a strict field.  Instead you must act *globally*, taking care to
always use the strict interface rather than the lazy one.  For space
leak free programming maps containing unevaluated thunks are illegal
states: we don't want them to occur in our program.  One of the
paradigmatic goals of strongly typed functional programming is to
[make illegal states
unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/).
It is unfortunate that it is not possible to follow the paradigm for
space leak free programming with `Data.Map`.

How about creating a new data type that is genuinely distinct from
`Data.Map.Lazy.Map`?  A simple `newtype` would suffice, as long as we
are careful not to expose the constructor and we only export functions
that preserve the "legality" of the state: when the map is evaluated
all the contents should be evaluated.  That is possible but it has
drawbacks.  For example the type would technically no longer obey the
functor law `fmap f . fmap g == fmap (f . g)`: for `f = const ()` and
`g = const undefined` then `fmap f . fmap g` would be `undefined` on a
strict map but `fmap (f . g)` wouldn't be.  (`Data.Map` has [separate
rewrite rules for strict and lazy
`map`](https://www.stackage.org/haddock/lts-13.21/containers-0.6.0.1/src/Data-Map-Strict-Internal.html#map)
for this reason.)

[EDIT: [/u/sjakobi on Reddit pointed
out](https://www.reddit.com/r/haskell/comments/q16haw/datamapstrictmap_is_not_a_strict_map/hfd2ww3/)
that such a package already exists:
[`strict-containers`](https://hackage.haskell.org/package/strict-containers)]

So what should be done?  I really don't know. My best idea is, as
above, to create a strict `newtype`.  Does this have other drawbacks?
Is there an alternative way to achieve space leak free programming
whilst `Data.Map.Strict` remains only an interface?  Is there another
solution?  If you know then please [tell
me](http://web.jaguarpaw.co.uk/~tom/contact).
