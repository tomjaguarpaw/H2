# extensible-effects, monad transformers and interleaving effects

## Introduction

In their paper ["Extensible
Effects"](http://okmij.org/ftp/Haskell/extensible/index.html),
Kiselyov, Sabry and Swords (KSS) introduce an effect system for
Haskell that is an alternative to the classical monad transformers
approach.  There is [a Haskell library of the same
name](http://hackage.haskell.org/package/extensible-effects)
implementing the ideas of the paper.  [Dan Doel noted in a recent
article](https://www.fpcomplete.com/user/dolio/monad-transformers-and-static-effect-scoping)
that this is one of many papers which investigate effects and their
associated handlers, in large part aiming to move from what is seen to
be the "static" approach of monad transformers, to the supposedly more
"dynamic" approach of algebraic effects.

Dan's article is a defence of the classical monad transformers
approach, and he argues convincingly "that the staticness [of monad
transformers] can actually be a useful property, that certain ways of
harnessing it can make the confusing constructions less so, and
... that algebraic effects perhaps aren't so different from what we
are already using".

KSS give two examples where they claim their approach is superior to
monad transformers.  The first is that it is awkward to combine
exceptions with non-determinism.  However, Dan implements a simple
alternative to `catchError` and ensures that the exception handler
removes from the set of effects.  In this way he efficiently disposes
with the supposed awkwardess.  The second objection of KSS is that a
certain interleaving of a read effect with coroutines is *impossible*
to achieve with monad transformers.  In this article I will
demonstrate that contrary to KSS's assertion, this interleaving *is*
possible with monad transformers, and moreover straightforward.

## Non-determinism and exceptions

`catchError` from the [mtl](http://hackage.haskell.org/package/mtl)
runs a monad transformer stack that contains an "exception raising"
layer and handles any exceptions with a handler that can potentially
rethrow them.  The definition has the following signature

    catchError :: MonadError e m => m a -> (e -> m a) -> m a

The monad `m` is the same in the argument and return value, thus any
exceptions that are rethrown are rethrown at exactly the same level of
the stack.  As KSS note, this *does* result in a particular
inflexibility when mixing effects.

However, Dan realised that handlers should *remove* from the set of
effects.  That is, the monad of the return value should be "smaller"
than the monad of the argument.  [He defines
`localCatch`](https://www.fpcomplete.com/user/dolio/monad-transformers-and-static-effect-scoping#throw-catch-confusion)
with the signature

    localCatch :: Monad m => ErrorT e m a -> (e -> m a) -> m a

which handles exceptions at the outermost level and gives the user the
chance to rethrow them at a lower level.  Using this formulation
yields the results that KSS are seeking with no further difficulties.

## Read effects and coroutines

KSS provide the following type `CoT` which is a co-routine transformer

    type CoT a m = ContT (Y m a) m
    data Y m a = Done | Y a (() -> m (Y m a))

and define a function which mixes read effects with coroutines.  The
idea is to use `local` to modify the local state of a read operation.
`local` has the signature

    MonadReader r m => (r -> r) -> m a -> m a

and the function is

    th3 :: MonadReader Int m => CoT Int m ()
    th3 = ay >> ay >> local (+10) (ay >> ay)
        where ay = ask >>= yield

They demonstrate that this has unexpected behaviour.  They also
consider the opposite order of effects

    th4 :: Monad m => ReaderT Int (CoT Int m) ()
    th4 = ay >> ay >> local (+10) (ay >> ay)
        where ay = ask = lift . yield

but this does not work either.  However, what I learned from Dan's
article is that handlers should *remove* from the set of effects, so
instead of `local` we can try implementing `localLocal` (awkwardly
named to be consistent with Dan's terminology!) which removes the read
effect.

    localLocal :: MonadReader a m => (a -> r) -> ReaderT r m b -> m b
    localLocal f m = ask >>= runReaderT m . f


Using `localLocal` highlights the problem.  The `ay` outside the
`localLocal` needs to have a different type than the `ay` *inside* the
`localLocal`.

    th3_explicit :: Monad m => CoT Int (ReaderT Int m) ()
    th3_explicit = ay1 >> ay1 >> localLocal (+10) (ay2 >> ay2)
     where ay1 :: Monad m => CoT Int (ReaderT Int m) ()
           ay1 = lift ask >>= yield
           ay2 :: Monad m => ReaderT Int (CoT Int m) ()
           ay2 = ask >>= lift . yield

This implementation gives exactly the results that KSS are looking
for.  Furthermore it's not hard to remove the duplication by defining
a monad transformer class `MonadCo` which abstracts over continuation
monads and provides a `yieldG` operation (a transformer-polymorphic
version of `yield`).  This leads to a neat and general implementation

    th3_MTL :: (MonadCo Int m, MonadReader Int m) => m ()
    th3_MTL = ay >> ay >> localLocal (+10) (ay >> ay)
    where ay :: (MonadCo r m, MonadReader r m) => m ()
          ay = ask >>= yieldG

## Conclusion

With the right primitives, combining complicated effects using monad
transformers can be straightforward.  What I've presened here is not
evidence that monad transformers are the *best* way to go, but it does
show that more thought is needed before we declare them obsolete!

----
