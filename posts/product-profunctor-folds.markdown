# ProductProfunctor folds

## Introduction

In an excellent piece of pedagogy, [Gabriel Gonzalez explains how to
implement composable streaming (space leak free) folds in
Haskell](http://www.haskellforall.com/2013/08/composable-streaming-folds.html).
In this article I'll explain how to make these composable folds even
more composable!

Gabriel's key idea is captured in [his foldl
package](http://hackage.haskell.org/package/foldl), where the notion
of a left fold is abstracted into the `Fold` datatype.

    data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

A `Fold a b` represents a left fold on a list of `a`, updating a state
of (existential) type `x`, and eventually returning a final value of type `b`.

* The first component (of type `x -> a -> x`) reads a value of type `a` from the
list and updates the state `x`.
* The second component (of type `x`) is the start state.
* The third component (of type `x -> b`) maps the final state of type
  `x` to a final state of type `b`.

There is an `Applicative` instance for `Fold a` which combines two
separate folds into a single fold that combines their state.  In this
way we can avoid space leaks when running two separate folds on the
same list.  For example

    average = (/) <$> L.sum <*> L.genericLength
    L.fold average [1..10000000]

only walks the list once and does not leak space.

## Lists of pairs

Suppose I have a value of type `[(Bool, Double)]` and I want to run `and`
on the first component (returns `True` if all elements are `True`,
`False` otherwise), and `average` (as defined above) on the second
component.  The simple approach of projecting out each component and
running the folds separately is flawed.

    myList :: [(Bool, Double)]

    L.fold L.and (map fst myList)
    L.fold average (map snd myList)

This exhibits exactly the kind of space leaks we are trying to avoid.
The spine of `myList` is fully forced and kept around between the two
calls to `L.fold`.  There is a solution, and that is to use a
`Profunctor` instance.

## Profunctor

A `Profunctor` `p` is like a `Functor` with two arguments, but the
first is *contravariant* meaning that it acts as a receiver of
values.  The class definition for `Profunctor` is

    class Profunctor p where
        lmap :: (a -> a') -> p a' b -> p a  b
        rmap :: (b -> b') -> p a  b -> p a b'
        -- rmap is just like fmap on the right-hand type variable

(For a more in-depth look at `Profunctor` see [my 24 Days of Hackage
post on the
subject](http://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html)).

`Fold` is a `Profunctor`, because `Fold a b` *receives* values of type
`a` and *emits* a value of type `b`.  Using the `Profunctor` instance
we can combine two `Fold`s of different argument types into one:

    (***!) :: Profunctor p => p a a' -> p b b' -> p (a, a') (b, b')
    p ***! p' = (,) <$> lmap fst p <*> lmap snd p'

    andWithAverage :: L.Fold (Bool, Double) (Bool, Double)
    andWithAverage = L.and ***! average

Now we can run `L.fold andWithAverage myList` *without* space leaks!

## ProductProfunctor

One could write a typeclass to capture the essence of what we have
just implemented

    class Profunctor p => ProductProfunctor p where
      (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
      empty :: p () ()

`(***!)` can be implemented exactly as given above, and `empty` is
just `pure ()`.  So what is the point of using a typeclass rather than
simply using the `Applicative` and `Profunctor` instances?  The class
ensures that the `Applicative` instance for `p a` is *independent* of
a.  I'm not *certain* this is necessary, but it does seem to be a
useful sanity check.

## Conclusion

The `Profunctor` instance for `Fold` increases the composability of
Gabriel's composable streaming folds.  The `ProductProfunctor` is an
interesting concept.

The `foldl` library doesn't actually supply a `Profunctor` instance,
but [Edward Kmett's similar `folds`
library](http://hackage.haskell.org/package/folds) does have
`Profunctor` instances.

----
