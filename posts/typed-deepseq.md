# Typed `deepseq`

Haskell's [deepseq](https://hackage.haskell.org/package/deepseq)
package contains
[`Control.DeepSeq.deepseq`](https://hackage.haskell.org/package/deepseq)
a function for "fully evaluating" a value.  `deepseq` is available for
each instance of `NFData`. Its exact behaviour is determined by the
instance but it is understood that "fully evaluating" means walking
through the entire value and evaluating, or "forcing", any thunks
(unevaluated computations) that are encountered.

In practical terms the function `deepseq` doesn't actually occur as a
method of `NFData`.  Instead the sole class method is `rnf :: NFData a
=> a -> ()` ("reduce to normal form").  Nevertheless, `rnf` and
`deepseq` are interchangeable via

```haskell
deepseq a b = rnf a `seq` b
rnf a = deepseq a ()
```

`rnf` is analogous to a similar function `rwhnf` ("reduce to weak head
normal form") that the library also provides.  This is just another
way of writing `seq`.

```haskell
rwhnf a = seq a ()
seq a b = rwhnf a `seq` b -- Naturally this can't be the definition of seq!
```

`seq` is built into the language.  It provides primitive functionality
that couldn't be written if the language itself didn't support it.
The language provides special syntax for an equivalent form,
specifically

```haskell
{-# LANGUAGE BangPatterns #-}

let !v = rhs in body
```

is defined to mean

```haskell
let v = rhs in v `seq` body
```

Personally I find the bang pattern form easier to understand than the
`seq` form.  It can be read as "when you encounter the binding for `v`
evaluate `rhs` before you evaluate `body`".  In contrast to `seq`,
`deepseq` and `rnf` do *not* need to be built in to the language.
They can be expressed in terms of `seq` and other language features as
follows.

* On an "atomic" value like `Int`, `Float`, `Bool` or `()`, `rnf` is the
  same as `rwhnf`, i.e. `deepseq` is the same as `seq`.

  ```haskell
  instance NFData Int   where rnf = rwhnf
  instance NFData Float where rnf = rwhnf
  instance NFData Bool  where rnf = rwhnf
  instance NFData ()    where rnf = rwhnf
  ```

  Furthermore, since `seq` cannot "look inside" functions the `NFData`
  instance for functions is just

  ```haskell
  instance NFData (a -> b) where rnf = whnf
  ```

* On a product type `rnf` applies `rnf` to all components of the
  product.  For example

  ```haskell
  {-# LANGUAGE BangPatterns #-}

  instance (NFData a, NFData b) => NFData (a, b) where
    rnf (a, b) =
      let !_ = rnf a
          !_ = rnf b
      in ()
  ```

  (This isn't actually how the `NFData` instance for tuples is written
  but is equivalent and easier to understand.)

* On a sum type `rnf` applies `case` to determine the constructor
  and then recursively `rnf`s what it finds inside.  For example

  ```haskell
  {-# LANGUAGE LambdaCase #-}

  instance (NFData a, NFData b) => NFData (Either a b) where
    rnf = \case
      Left a  -> rnf a
      Right b -> rnf b
  ```

  (Again this isn't the actual implementation but is equivalent.)

* For a value of a recursive type will walk the structure recursively
  applying `rnf` to what it encounters.  For example

  ```haskell
  instance NFData a => NFData [a] where
    rnf = \case
      a:as -> let !_ = a in rnf as
      []   -> ()
  ```

More sophisticated data structures constructed from more complicated
primitives, such as arrays, bytestrings and maps, have their own
specialised implementations of `NFData` but they're not particularly
important here.

Why do we want `deepseq`?
