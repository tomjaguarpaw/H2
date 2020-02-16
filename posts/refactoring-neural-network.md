# Refactoring a neural network implementation in Haskell

In 2015, [Ben Lynn](http://www-cs-students.stanford.edu/~blynn/)
[wrote a digit-classifying neural network in
Haskell](http://www-cs-students.stanford.edu/~blynn/haskell/brain.html)
that is remarkable in two ways.  Firstly, he wrote the backpropagation
code by hand rather than relying on a framework.  Secondly, he wrote
in a terse, point-free-heavy style that must have taken great mental
acuity.  I came across Ben's article on [Hacker
News](https://news.ycombinator.com/item?id=22315668) where it was
suggested that, unfortunately, the style does not make it easy for the
unaccustomed reader to understand.  Haskell makes refactoring safe and
convenient so let's try and improve the readability of the code!

The original code appears in [Ben's
article](http://www-cs-students.stanford.edu/~blynn/haskell/brain.html)
and in [a Github
repository](https://github.com/blynn/morans/blob/bd02edc60a5048c3c109fe76adc1842691c664cd/morans.hs).
Let's warm up by tackling something small.  `zLayer` implements a
layer of a neural network.

```haskell
zLayer ::[Float] -> ([Float], [[Float]]) -> [Float])
zLayer as (bs, wvs) =
  zipWith (+) bs $ sum . zipWith (*) as <$> wvs
```

How does it work and what can we improve?  Before we go any further I
want to be confident that the changes that I am making are correct.
Refactoring "[is the process of restructuring existing code without
changing its external
behavior](https://en.wikipedia.org/wiki/Code_refactoring)" so to be
sure that I am not changing the external behaviour I will use a test
harness implemented with the excellent
[Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog/) property
testing library.

## Hedgehog

I'm going to make an exact duplicate of the `zLayer` function called
`zLayer_new`.  Then I will proceed to refactor it whilst a Hedgehog
check tests that the refactored version has exactly the same behaviour
as the original version.  The Hedgehog code is

```haskell
-- Generate a random list
floatList :: MonadGen m => m [Float]
floatList = Gen.list (Range.linear 0 10)
                     (Gen.float (Range.linearFrac (-10) 10))

-- Generate random inputs for zLayer
zLayerInput :: MonadGen m => m ([Float], ([Float], [[Float]]))
zLayerInput = do
  l1 <- floatList
  l2 <- floatList
  ll <- Gen.list (Range.linear 0 10) floatList

  return (l1, (l2, ll))

-- Check that my new implementation matches the original
prop_same_zLayer :: Property
prop_same_zLayer = property $ do
  (l1, t) <- forAll zLayerInput
  zLayer_new l1 t === zLayer l1 t

-- Run all the tests
tests :: IO Bool
tests = checkSequential $$(discover)
```

`prop_same_zLayer` checks that the old and new implementation of
`zLayer` match.  After every refactoring I'm going to run my Hedgehog
tests to ensure that I haven't changed any behaviour.

## Use domain-specific operators and types

Now let's get back to looking at `zLayer`.  I notice that `zipWith
(+)` performs the operation of adding two vectors.  The vectors are
represented by lists of `Float`, and `zipWith (+)` adds the
corresponding entries in two lists, which is the same thing that
vector addition does.  This suggests my first refactoring.  I'm going
to define a new operator for vector addition.

```haskell
(.+) ::[Float] -> [Float] -> [Float])
(.+) = zipWith (+)

zLayer_new :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer_new as (bs, wvs) = bs .+ (sum . zipWith (*) as <$> wvs)
```

If `zipWith (+)` is vector addition then what is `zipWith (*)`?
Componentwise multiplication followed by summing the components is the
definition of the dot product of two vectors so I'll introduce a `dot`
operation.

```haskell
dot :: [Float] -> [Float] -> Float
dot v1 v2 = sum (zipWith (*) v1 v2)

zLayer_new ::[Float] -> ([Float], [[Float]]) -> [Float]
zLayer_new as (bs, wvs) = bs .+ (dot as <$> wvs)
```

What about this `<$>` thing?  That's another name for `fmap`, and it's
mapping the `dot as` function across the list-of-lists called `wvs`.
What does that mean?  One way of interpreting vector-matrix
multiplication is that it maps the dot operation across the columns of
a matrix.  Therefore I add a "vector-matrix multiplication" operation,
`.*`.

```haskell
(.*) :: [Float] -> [[Float]] -> [Float]
v .* m = dot v <$> m

zLayer_new :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer_new as (bs, wvs) = bs .+ (as .* wvs)
```

I'm going to take advantage of the knowledge gained during refactoring
to introduce some type synonyms.

```haskell
type Vector = [Float]
type Matrix = [[Float]]

zLayer_new :: Vector -> (Vector, Matrix) -> Vector
zLayer_new as (bs, wvs) = bs .+ (as .* wvs)
```

Compared to the original `zLayer` (reproduced below) we have made a
big improvement.  There is no `sum`, no `zipWith` and no
non-mathematical operator.  `zLayer` is an operation which multiplies
by a matrix and then adds a vector.  It's as simple as A * B + C.

```
zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs
```

## Introduce new variables when it helps

I'm pleased with how `zLayer` looks so I will go straight to the next
mathematical function in the module, called `feed`.

```haskell
feed_new :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl' (((relu <$>) . ) . zLayer)
```

The type is similar to that of `zLayer` but I'm not going to use my
`Vector` and `Matrix` synonyms until I'm sure that they are indeed
what the lists of `Float`s are representing. (I'll keep adding
Hedgehog equivalence tests but won't show any more of the Hedgehog
code in this article because it's completely standard testing code.)

The use of `<$>` and `.` is mysterious.  I know that `<$>` is `fmap`
and `.` is function composition but that doesn't help me understand
what they are doing.  Use of function composition is popular in
Haskell to avoid naming intermediate variables.  Sometimes avoiding
intermediate variables helps readability but here I think it hurts
readability so I'm going to reintroduce them.

I'm going to expand `.` by replacing it with its definition.  I know
that the first argument of `zLayer` is a vector so I choose the name
`v` for the variable of the lambda.  Naturally, after each refactoring
I run my Hedgehog tests to check that `feed_new` has the same
behaviour as `feed`.

```haskell
feed_new = foldl' (\v -> ((relu <$>) . ) (zLayer v))
```

That's a good start.  Then I apply the `.` section

```haskell
feed_new = foldl' (\v -> (relu <$>) . zLayer v)
```

and I can inline the definition of `.` again.  The second argument of
`zLayer` is a `(Vector, Matrix)` so I choose the name `vm` for
the lambda.

```haskell
feed_new = foldl' (\v vm -> relu <$> zLayer v vm)
```

This is *much* better!  Just from knowing what a left fold is we can
understand that `feed` starts with a vector and then successively
applies `zLayer` and `relu` for each `vm` in a list.  At this point I
know it's fine to use the type synonyms I defined earlier. I'm also
going to use my domain knowledge to define a new type synonym,
`Layer`. A neural network consists of a sequence of `Layer`s, each
layer comprising a `Vector` and `Matrix`.  We started with

```
feed = foldl' (((relu <$>) . ) . zLayer)
```

and we ended with

```haskell
type Layer = (Vector, Matrix)

feed_new :: Vector -> [Layer] -> Vector
feed_new = foldl' (\v vm -> relu <$> zLayer vm m)
```

That is, `feed` takes a vector, applies a sequence of neural network
layers and returns the resulting vector.

## Make properties structural

Right, onto the next candidate, `revaz`.

```haskell
revaz_new :: [Float]
          -> [([Float], [[Float]])]
          -> ([[Float]], [[Float]])
revaz_new xs =
  foldl' (\(avs@(av:_), zs) (bs, wms) ->
             let zs' = zLayer av (bs, wms)
             in ((relu <$> zs'):avs, zs':zs)) ([xs], [])
```

Something looks suspicious here. There is a partial pattern match on a
list.  What happens if the list is empty?  To our relief we can see
that the list starts non-empty (it is `[xs]`) and it only ever grows
(in each iteration it has `relu <$> zs'` consed onto the front).  This
is all very well but personally I'd prefer that it was the compiler,
rather than my own reasoning, that reassures me about safe program
behaviour.  Let's pass the "head of the list" in a separate tuple
component.  We'll have to cons the final "head of the list" onto the
list after the `foldl'` has completed.

```haskell
revaz_new xs ys =
  let (av, avs, zs) =
        foldl' (\(av, avs, zs) (bs, wms) ->
                  let zs' = zLayer av (bs, wms)
                  in (relu <$> zs', av:avs, zs':zs))
             (xs, [], [])
             ys
  in (av:avs, zs)
```

This looks a bit more messy than before but let's keep going and see
where we can get.  The next thing I notice is that each iteration
around the loop we stick one new element onto the front of each of
`avs` and `zs`, that is, at all stages `avs` and `zs` are exactly the
same length.  Let's make this property structural by building one list
of pairs instead of two lists.  Again, we'll have to postprocess the
result of `foldl'`, this time unzipping one list into two lists.

```haskell
revaz_new xs ys =
  let (av, avs_zs) =
        foldl' (\(av, avs_zs) (bs, wms) ->
                  let zs' = zLayer av (bs, wms)
                  in (relu <$> zs', (av, zs'):avs_zs))
             (xs, [])
             ys
      (avs, zs) = unzip avs_zs
  in (av:avs, zs)
```

Now that I can see what's going on a bit more clearly I can add type
synonyms to the signature.  Importantly, I can see that the
`[[Float]]`s are not `Matrix`s, rather they are lists of `Vector`s!
It's good that I was cautious and didn't just substitute type synonyms
blindly.

```haskell
revaz_new :: Vector
          -> [Layer]
          -> ([Vector], [Vector])
revaz_new xs ys = (av:avs, zs)
  where (avs, zs) = unzip avs_zs
        (av, avs_zs) =
          foldl' (\(av, avs_zs) (bs, wms) ->
                    let zs' = zLayer av (bs, wms)
                    in (relu <$> zs', (av, zs'):avs_zs))
                 (xs, [])
                 ys
```

Finally I can neaten this a little by noticing that I don't need to
unpack the `(bs, wms)` tuple.

```haskell
revaz_new :: Vector
          -> [Layer]
          -> ([Vector], [Vector])
revaz_new xs ys = (av:avs, zs)
  where (avs, zs) = unzip avs_zs
        (av, avs_zs) =
          foldl' (\(av, avs_zs) t ->
                    let zs' = zLayer av t
                    in (relu <$> zs', (av, zs'):avs_zs))
                 (xs, [])
                 ys
```

I find this marginally clearer than what we started with but not
ideal.  Let's move on and we'll come back to `revaz` later.

## Try to avoid explicit recursion

Next up, this behemoth.

```haskell
deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = revaz xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [sum $ zipWith (*) row dv | row <- wm] (relu' <$> zv)
```

The Hedgehog equivalence test requires some care.  As you can see from
the pattern match on the result of `revaz`, `deltas` expects both the
lists in the pair to be non-empty.  The first one, `avs`, is
guaranteed to be non-empty (we can conveniently see this from our
refactored version of `revaz`: it has `av` on the front).  What about
the second one, `zvs`?  By inspecting `revaz` we can see that the
length of `zvs` is the same as the length of the list-of-`Vector`s
input to `revaz`.  Therefore we need `layers` to be non-empty. If we
tell Hedgehog to generate input data satisfying this condition then
all is well.

The first thing in `deltas` that I want to tackle is the local
function `f`.  It looks complicated, involving `zipWith`, a `:`
section and a list comprehension.  Even worse, it's a recursive
function!  Even though recursion is typically thought of as the
bread-and-butter of functional programming, unrestricted recursion is
almost as bad for code comprehensibility as GOTO is in imperative
programming.  We ought to seek to use recursion combinators such as
maps, folds, scans, etc. in preference to direct recursion.

Let's tackle the recursion shortly.  I see something I can do straight
away, which is to use the `dot` product operator that I defined
earlier.

```haskell
f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
   zipWith (*) [dot row dv | row <- wm] (relu' <$> zv)
```

`dot` is commutative, so I can write `dot row dv` as `dot dv row`.
Then I can conveniently rewrite the list comprehension as an `fmap`.

```haskell
f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
  zipWith (*) (fmap (dot dv) wm) (relu' <$> zv)
```

Recalling that `<$>` is an alias for `fmap` we see that the list
comprehension was just doing a vector-matrix product all along!

```haskell
f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
  zipWith (*) (dv .* wm) (relu' <$> zv)
```

This is looking a lot better already, but still not good.  There's a
mysterious `:` section there.  Let's just inline it.

```
f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $
  zipWith (*) (dv .* wm) (relu' <$> zv) : dvs
```

That looks less mysterious and the line is now readable.  The function
`f` is still mysterious though.  What's it doing?  As in `revaz`, it
unconditionally inspects the first element, `dv`, of a list, so let's
pass that in as a separate argument.

```haskell
  in (reverse avs,
      f (transpose . snd <$> reverse layers) zvs delta0 []) where
    f _ [] dv dvs = dv:dvs
    f (wm:wms) (zv:zvs) dv dvs = f wms zvs
      (zipWith (*) (dv .* wm) (relu' <$> zv)) (dv:dvs)
```

Next I notice that `f` is iterating over two lists at the same time,
taking one element off each during each iteration.  It may as well
iterate over the `zip` of the two lists instead!  Why is that a good
thing to do?  Because `f` is now nearly a left fold.  Let's make it
closer to left fold by packaging the `dv` and `dvs` together in a
tuple

```haskell
in (reverse avs,
    f (zip (transpose . snd <$> reverse layers) zvs) (delta0, []))
where
  f [] (dv, dvs) = dv:dvs
  f ((wm, zv):wms_zvs) (dv, dvs) = f wms_zvs
    (zipWith (*) (dv .* wm) (relu' <$> zv), dv:dvs)
```

and combine `dv` and `dvs` after the recursive function (now called
`g`) has returned, rather than in the base case.

```haskell
f l t = let (dv, dvs) = g l t in dv:dvs

g [] (dv, dvs) = (dv, dvs)
g ((wm, zv):wms_zvs) (dv, dvs) = g wms_zvs
  (zipWith (*) (dv .* wm) (relu' <$> zv), dv:dvs)
```

Now `g` *is* a left fold!

```haskell
g l t = foldl' h t l
   where h (dv, dvs) (wm, zv) =
           (zipWith (*) (dv .* wm) (relu' <$> zv), dv:dvs)
```

Shuffling some arguments around and tidying gives us

```haskell
f l t = let (dv, dvs) = g t l in dv:dvs

g = foldl' (\(dv, dvs) (wm, zv) ->
      (zipWith (*) (dv .* wm) (relu' <$> zv), dv:dvs))
```

This is already looking *much* better than the `f` that we started
with, but additionally, something excellent has occurred.  We can see
that both `revaz_new` and `g` employ the same sort of recursion
pattern.  They both iterate over a list with a state and push one new
value onto the front of a list each iteration (treating the list as a
sort of stack).  We can capture this recursion pattern by abstracting
the behaviour out into a recursion combinator!  I'm going to call it
`revMapWithState`, a name that describes fairly well what it does.

```haskell
revMapWithState :: (state -> a -> (state, stack))
                -> (state, [stack])
                -> [a]
                -> (state, [stack])
revMapWithState f =
  foldl' (\(state, stack) item ->
     let (nextState, nextStack) = f state item
     in (nextState, nextStack:stack))
```

`revMapWithState` captures the sort of recursion that's happening in
`revaz_new` and `g`, and we can rewrite both of them in terms of it.

```haskell
revaz_new :: Vector
          -> [Layer]
          -> ([Vector], [Vector])
revaz_new xs ys = (av:avs, zs)
  where (avs, zs) = unzip avs_zs
        (av, avs_zs) =
          revMapWithState (\av t ->
                    let zs' = zLayer av t
                    in (relu <$> zs', (av, zs')))
                 xs
                 ys
...

g = revMapWithState (\dv (wm, zv) ->
      (zipWith (*) (dv .* wm) (relu' <$> zv), dv))
```

The benefit of using `revMapWithState` is that it takes care of the
result stack for us and we only have to produce the next element of
the stack and the next state.

Looking at where we are now with `deltas_new`, I can see that
expressions of the form `zipWith (*) _ (relu' <$> _)` appear twice.
Twice [isn't quite enough on its
own](https://en.wikipedia.org/wiki/Rule_of_three_(computer_programming))
to make us want to pull it out into a separate function, but my domain
knowledge that this is the backpropagator for
[`relu`](https://en.wikipedia.org/wiki/Rectifier_(neural_networks))
convinces me that this is the right thing to do.  Now we can tidy up,
using a little domain knowledge in naming the variables, to achieve

```haskell
dRelu :: Vector -> Vector -> Vector
dRelu a b = zipWith (*) a (relu' <$> b)

deltas_new :: Vector
           -> Vector
           -> [Layer]
           -> ([Vector], [Vector])
deltas_new xv yv layers = (reverse avs, dv:dvs) where
  (avs@(av:_), zv:zvs) = revaz xv layers
  dv0 = dRelu (zipWith dCost av yv) zv
  matrices = map snd layers
  (dv, dvs) =
    backProp dv0 (zip (map transpose (reverse matrices)) zvs)
  backProp =
    revMapWithState (\dv (wm, zv) -> (dRelu (dv .* wm) zv, dv))
```

It's still not *easy* to read, but the reason that it is hard to read
has changed.  It is now hard to read because it's a hand-written
backpropagation routine for a deep neural network.  It used to be hard
to read because it contained deeply-nested expressions,
non-mathematical symbols and an ad hoc recursive function.  I think
that the refactored version is about as easy to read as the overall
design of this module will allow.

We've improved the readability of the original program significantly
so I'll stop here.  Whilst I think we have improved Ben's program I'm
also impressed that he was able to write it the way he did in the
first place! I couldn't have kept all the necessary details in my
head.  There's more we could refactor but this article already gives
us plenty to chew on.  If you enjoyed this article and you'd like me
to continue refactoring in a future article then please and [let me
know](http://web.jaguarpaw.co.uk/~tom/contact/).

## Conclusion

Haskell makes refactoring a breeze.  Higher-order functions and
expression-based style make it possible to slice and splice code in
fine grained ways and easily capture repeated patterns.
[Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog/) allows us
to [refactor mercilessly](http://wiki.c2.com/?RefactorMercilessly)
whilst remaining confident that we are not changing program behaviour.
The examples of refactorings we saw that make code easier to
understand were: using domain-specific operators and types, introducing
new variable names when it helps readability, converting program
properties (which can't be checked by a type checker) into program
structure (which can) and avoiding explicit recursion where possible.

If you come across Haskell code in the wild that you can't easily read
then do not be disheartened.  Maybe it can be refactored using the
techniques described in this article.

### Other examples of refactoring Haskell

See also my other worked examples of refactoring in Haskell:

* [Good design and type safety in
   Yahtzee](../good-design-and-type-safety-in-yahtzee/)

* [Using our brain less in refactoring
   Yahtzee](../using-brain-less-refactoring-yahtzee/)
