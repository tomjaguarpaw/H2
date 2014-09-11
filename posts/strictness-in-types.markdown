# Reflecting strictness in Haskell types

In GHC, from the operational point of view, the type `Int` does not
indicate a bit pattern somewhere in memory that represents an integer.
What it indicates is *either* an integer bit pattern *or* a "thunk" (a
delayed computation) which can be "forced" (run).  When and if the
computation terminates the thunk will be overwritten with the integer
bit pattern that it produced.

Thus the GHC type system provides less fine-grained information than
we might naively expect, especially if we are used to the strict point
of view.  Often the ability to invisibly interchange thunks and values
is a blessing.  In fact it is one of Haskell's great blessings.
However, it is well known that it can also be a curse.  Here's an
example:

    foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl _ z [] = z
    foldl f z (b:bs) = foldl f (f z b) bs

The application of `f` to `z` and `b` does not result in an evaluated
`a`.  Instead it results in a thunk representing the function call.
If the input list is of length `n` we make `n` recursive calls and
build up a thunk whose size is proportional to `n`.  This leaks space.

The way to avoid a space leak is to explicitly force the `a` we have
created with a call to `seq` before each recursive call.

    foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' _ z [] = z
    foldl' f z (b:bs) = a `seq` foldl f a bs
        where a = f z b

This way the `a` remains completely evaluated at each stage.

This is a reasonable solution, but as Haskellers we are used to our
types specifying very fine-grained information about the operation of
our program.  We want our types to specify when `IO` actions can
occur, so why not ask for our types to specify when evaluation of
thunks takes place?

Unfortunately this isn't straightforward to come by.  For example, we
might try

    data Strict a = Strict !a

    foldl :: (a -> b -> a) -> Strict a -> [b] -> a
    foldl _ (Strict z) [] = z
    foldl f (Strict z) (b:bs) = foldl f (Strict (f z b)) bs

But this is no good.  After all, a `Strict a` doesn't represent a
strict `a` but rather a *thunk* returning a strict `a`!  So we build
up a chain of thunks nonetheless.  In fact there is no difference in
terms of laziness between `a` and our attempted `Strict a`.  Both of
them are just thunks that can be forced to return an `a`.

However, there is a trick that allows us to represent strictness in
types.  The trick is to indicate strictness in the *argument* to a
function rather than its return type.  I think I learned this trick
from a post to the haskell-cafe mailing list, but I cannot find it now
so I don't know who to give credit to.

    {-# LANGUAGE TypeOperators #-}
    
    data a :-> b = Strict (a -> b)
    -- Strict constructor will be hidden

    -- Specifying the precedence of :-> merely for neater syntax
    infixr 0 :->

    strictly :: (a -> b) -> a :-> b
    strictly f = Strict (\a -> a `seq` f a)
    
    (!) :: (a :-> b) -> a -> b
    Strict f ! a = f a

Then we can write a recursive `foldl` which is guaranteed to be space
leak free because it has the right type.

    foldl' :: (a -> b -> a) -> a :-> [b] -> a
    foldl' f = strictly (\z xs -> case xs of
                  []   -> z
                  y:ys -> foldl' f !(f z y) $ ys)

The value whose type is on the left hand side of a `:->` is guaranteed
to be evaluated strictly, i.e. forced before the result can start to
be consumed.

## Polymorphic strictness

What do we do if we want to define functions which are polymorphic
over strictness type?  For example the standard definition of `const`
doesn't touch its `b` argument when its return value is evaluated.

    const :: a -> b -> a
    const a b = a

On the other hand we can define another version which does

    const' :: a -> b -> a
    const' a b = b `seq` a

How do we implement both of these with one definition?  Well, we can introduce
a class to capture the general concept with instances for `(->)` and
`(:->)`.  (The need for the notation `arrow` instead of something
symbolic is a syntactic annoyance.  I'm not sure how to get round it.)

    class FunctionLike arrow where
      (?) :: (a `arrow` b) -> a -> b
      functionLike :: (a -> b) -> (a `arrow` b)

    instance FunctionLike (->) where
      (?) = id
      functionLike = id
    
    instance FunctionLike (:->) where
      (?) = (!)
      functionLike = strictly

Then we can define polymorphic `const` as 

    constPolymorphic :: FunctionLike arrow => a -> b `arrow` a
    constPolymorphic a = functionLike (\_ -> a)

and use it to directly derive the two specialisations that we want.

    const :: a -> b -> a
    const = constPolymorphic
    
    const' :: a -> b :-> a
    const' = constPolymorphic

If we were so inclined we could use `FunctionLike` to define the leaky
and leak free versions of `foldl` at the same time!

    foldlPolymorphic :: FunctionLike arrow =>
                        (a -> b -> a) -> a `arrow` ([b] -> a)
    foldlPolymorphic f = functionLike (\z xs -> case xs of
                             []   -> z
                             y:ys -> foldl' f ?(f z y))

The operator `?` should be interpreted as function application which
is polymorphic over strictness.

## Strictness in data types

Even more amazingly, [gelisam on Haskell Reddit showed me how to make
data types that are strictness
polymorphic!](http://www.reddit.com/r/haskell/comments/2chb2h/fantasy_world_haskell/cjgepwj)
The key observation is that the only difference between the usual,
lazy `data Foo a = Foo a` and the strict `data Foo a = Foo !a` is that
the function which constructs `Foo`s is lazy in its argument for the
former, and strict in its argument for the latter.

If we want to be polymorphic over strict and lazy lists, for example,
we can define

    data List (arrow :: * -> * -> *) a = Nil | Cons a (List arrow a)
    -- The List constructor will have to be hidden, although pattern
    -- matching on it would be OK.  arrow is a phantom type which we
    -- use as a strictness indicator.
    
    -- We expose this polymorphic cons instead
    cons :: FunctionLike arrow =>
            a `arrow` (List arrow a `arrow` List arrow a)
    cons = functionLike (\x -> functionLike (\y -> Cons x y))

(Again I apologise for the syntax.  Hopefully someone knowledgeable can
tell me what to do about that.)

A `List (->) a` is a lazy list of `a`, and a `List (:->) a` is a
strict list of `a`.  We can define data values in a way that is
polymorphic over strictness type, for example enumerating all integers
in a range.

    range :: FunctionLike arrow => Int -> Int -> List arrow Int
    range a b = if a > b
                then Nil
                else cons ?a ?(range (a+1) b)

(Recall that the operator `?` should be interpreted as function
application which is polymorphic over strictness.)

Then `count 1 1000000 :: List (->) Int` is a lazy list where the one
million elements are computed as required.  `count 1 1000000 :: List
(:->) Int` is a strict list where all one million elements are
computed at once.

## Conclusion

Strictness in Haskell is generally hidden away and not reflected by
types, but there does seem to be a way of representing strictness in
the type system!
