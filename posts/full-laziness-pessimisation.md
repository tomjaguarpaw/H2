# The full-laziness pessimisation

Work in progress: DO NOT CIRCULATE

## The full laziness transformation and why we like it

Chapter 15 of [The Implementation of Functional Programming
Languages](https://www.microsoft.com/en-us/research/uploads/prod/1987/01/slpj-book-1987.pdf)
by Simon Peyton Jones contains the following example

```haskell
let f = g 4
    g x y = y + sqrt x
in f 1 + f 2
```

The claim is that this should be transformed to

```haskell
let f = g 4
    g x = let sqrt_x = sqrt x
          in \y -> y + sqrt_x
in f 1 + f 2
```

Then we avoid evaluating `sqrt 4` twice.  Now, the programmer could
have written this transformation by hand if she had wanted to, but the
first version reads more clearly.  The transformation which takes the
first version to the second version is called the "full laziness
transformation".

The full laziness transformation is particularly useful when a lambda
has no free variables at all.  Reid Barton [gives a couple of
examples](https://stackoverflow.com/questions/35115172/why-is-full-laziness-a-default-optimization/35115664#35115664):

```haskell
g z = f (z+1)
  where f 0 = 0
        f y = 1 + f (y-1)
```

means

```haskell
g = \z -> let f 0 = 0
              f y = 1 + f (y-1)
          in f (z+1)
```

but `f` does not depend on `z` so it would be better to have instead

```haskell
f 0 = 0
f y = 1 + f (y-1)

g = \z -> f (z+1)
```

That way no closure for `f` need be allocated when `g` is called.
Similarly we like to write

```haskell
h xs = map (+1) xs
```

which means

```haskell
h xs = let plus_one = \x -> x + 1
       in map plus_one xs
```

but

```haskell
plus_one = \x -> x + 1

h xs = map plus_one xs
```

is more efficient, yet too ugly to write by hand.  Basically, without
the full laziness transformation we are liable to get death by a
thousand cuts.  Full laziness likely makes many small improvements
that add up to a macroscopic increase in compiled program speed.

## Why we don't like it

* <https://mail.haskell.org/pipermail/haskell-cafe/2015-December/122521.html>

* <https://mail.haskell.org/pipermail/ghc-devs/2014-August/006195.html>

* <https://gitlab.haskell.org/ghc/ghc/-/issues/8457>

  > In this bug report I'd like to argue that -ffull-laziness
  > shouldn't be turned on automatically with either -O nor -O2,
  > because it's dangerous and can cause serious memory leaks which
  > are hard to debug or prevent. I'll also try to show that its
  > optimization benefits are negligible. Actually, my benchmarks show
  > that it's beneficial to turn it off even in the cases where we
  > don't hit a space leak.

  This issue suggested disabling `full-laziness` under `-O2` and
  provided evidence that no harm would come.  Unfortunately discussion
  died out.

* <https://gitlab.haskell.org/ghc/ghc/-/issues/917>

* <https://gitlab.haskell.org/ghc/ghc/-/issues/1945>

* <https://gitlab.haskell.org/ghc/ghc/-/issues/7367>

* <https://github.com/michaelt/streaming/issues/6>

* <https://stackoverflow.com/questions/26321784/haskell-compiling-with-o2-drastically-increases-memory-usage/26322330#26322330>

* <https://stackoverflow.com/questions/32345095/what-is-the-correct-way-to-perform-constant-space-nested-loops-in-haskell/32359655#32359655>

* <https://www.reddit.com/r/haskell/comments/wz5gcq/why_is_there_a_leak/.compact>

## What to do

* [Sebastian Graaf
  says](https://stackoverflow.com/questions/35115172/why-is-full-laziness-a-default-optimization#comment89506961_35115664)
  "It's a shame misses a flag for that."

## See also

* <https://foldoc.org/Full+laziness>

* <https://www.reddit.com/r/haskell/comments/55xk4z/erratum_to_sharing_memory_leaks_and_conduit_and/>
