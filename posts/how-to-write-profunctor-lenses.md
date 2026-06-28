How to write profunctor lenses
===

-- and prisms

A profunctor optic has the following general form

```haskell
type LensLike p s t a b = p a b -> p s t
```

Compare this with the van Laarhoven version

```haskell
type LensLikeVL f s t a b = (a -> f b) -> s -> f t
```

The way we write lenses in profunctor form is to see `p a b` as some
sort of converter from `a` to `b` and transform it to a converter from
`s` to `t`.  Lenses work with the profunctor typeclass `Strong`,
defined by the following method:

```haskell
first' :: p a b -> p (a, c) (b, c)
```

That allows a converter from `a` to `b` to be transformed into a
converter from "`a` and some stuff" "`b` and some stuff", that is any
product containing an `a` that can be "converted to" a `b`!  That means
we can write a lens (in terms of `second'` rather than `first'` since
it makes the sequel slightly more convenient) by splitting `s` into
"`a` and some stuff" and feeding it into one end of the `Strong`
profunctor.  Then we assemble `t` on the other end from "`b` and some
stuff".

```haskell
_1 :: Strong p => p a a' -> p (a, b, c) (a', b, c)
_1 = dimap (\(a, b, c) -> ((b, c), a)) (\((b, c), a) -> (a, b, c)) . second'
```

In fact there are different representations for "stuff".  The stuff
could even have been the assembling function itself:

```haskell
_1' :: Strong p => p a a' -> p (a, b, c) (a', b, c)
_1' = dimap (\(a, b, c) -> (\a' -> (a', b, c), a)) (\(b_c, a) -> b_c a) . second'
```

This suggests the following, which turns out to be a way of creating a
profunctor lens from a getter/setter:

```haskell
lens :: Strong p => (s -> (b -> t, a)) -> p a b -> p s t
lens f = dimap f (uncurry ($)) . second'
```

Prisms work with the profunctor typeclass `Choice`, defined by the
following method:

```haskell
left' :: p a b -> p (Either a c) (Either b c)
```

This time we don't use "`a` and some stuff" but instead "`a` *or* some
stuff".

```haskell
_Just :: Choice p => p a a' -> p (Maybe a) (Maybe a')
_Just = dimap (\case {Nothing -> Right (); Just a -> Left a})
              (\case {Left a' -> Just a'; Right () -> Nothing})
        . left'
```

As before there's an equivalent way of presenting this

```haskell
_Just' :: Choice p => p a a' -> p (Either a t) ((a' -> t) -> t)
_Just' = rmap (\case {Left a' -> ($ a'); Right t -> const t})
         . left'
```

which leads to a way of creating a prism from a match and constructor.

```haskell
prism :: Choice p
      => (s -> Either a t) -> (b -> t) -> p a b -> p s t
prism f g = dimap f (\case {Left a' -> g a'; Right t -> t}) . left'
```
