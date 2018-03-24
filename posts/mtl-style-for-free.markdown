# MTL style for free

## Introduction

"MTL style" and "free monad style" are competing ways of implementing
classes of effectful operations in Haskell programs.  When we use MTL
style we specify a type class of monadic operations and give various
instances which interpret the operations in different monads.  When we
use free monad style we specify a `Functor` which encodes the monadic
operations and we implement various interpretations of that functor
in different monads.

After reading [an excellent article by Matt
Parsons](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)
(which advocated the MTL style) I realised that you can have the best
of both worlds!  The base functor that defines the free monad also
gives rise to an MTL style type class.

## Example

Inspired by Matthew's article on the three layer cake pattern in
Haskell, we might have the interfaces

```haskell
class MonadTime m where
  getCurrentTime :: m UTCTime

class MonadLock m where
    acquireLock :: NominalDiffTime -> Key -> m (Maybe Lock)
    renewLock :: NominalDiffTime -> Lock -> m (Maybe Lock)
    releaseLock :: Lock -> m ()
```

and give them interpretations which can be either effectful or pure
mocked versions

```haskell
acquireLockIO :: NominalDiffTime -> Key -> IO (Maybe Lock)
renewLockIO   :: NominalDiffTime -> Lock -> IO (Maybe Lock)
releaseLockIO :: Lock -> IO ()

instance MonadTime ((->) UTCTime) where
  getCurrentTime = id


instance MonadTime IO where
  getCurrentTime = Data.Time.getCurrentTime

instance MonadLock IO where
  acquireLock = acquireLockIO
  renewLock   = renewLockIO
  releaseLock = releaseLockIO
```

## MTL style for free

Instead of defining different classes for different sets of operations
let's define a class once and for all parameterized by a `Functor`
which carries the operations.

```haskell
class Monad_ f m where
  interpret :: forall r. f r -> m r
```

Then instead of `MonadTime` we have `Monad_ Time_` and instead of
`MonadLock` we have `Monad_ Lock_`, where

```haskell
data Time_ a = GetCurrentTime (UTCTime -> a)

data Lock_ a = AcquireLock (Maybe Lock -> a) NominalDiffTime Key
             | RenewLock (Maybe Lock -> a) NominalDiffTime Lock
             | ReleaseLock (() -> a) Lock
```

The way that we write a functor for a class of monadic operations is
that we take our collection of operations of the form

```haskell
class MonadOperations where
    operation1 :: arg1 -> ... -> argN -> m result
    ...
```

and convert it to a sum type of the form

```haskell
data Operations_ a = Operation1 (result -> a) arg1 ... argN
                   | ...
```

We can go ahead and give instance definitions for our classes

```haskell
instance Monad_ Time_ ((->) UTCTime) where
  interpret = \case
    GetCurrentTime f -> k0 id f


instance Monad_ Time_ IO where
  interpret = \case
    GetCurrentTime f -> k0 Data.Time.getCurrentTime f

instance Monad_ Lock_ IO where
  interpret = \case
    AcquireLock f t k -> k2 acquireLockIO f t k
    RenewLock f t l   -> k2 renewLockIO f t l
    ReleaseLock f l   -> k1 releaseLockIO f l
```

For each constructor we give its interpretation in the target monad.
`k0`, `k1` and `k2` are somewhat uninteresting combinators for making
this look cleaner.

```haskell
k0 op f = fmap f op
k1 op f a = fmap f (op a)
k2 op f a b = fmap f (op a b)
```

The downside of this new style is that if we want to use direct
function calls for our operations we have to wrap them:

```haskell
-- All these types are inferrable
-- with NoMonomorphismRestriction
getCurrentTime :: Monad_ Time_ m
               => m UTCTime
getCurrentTime = j0 GetCurrentTime

acquireLock :: Monad_ Lock_ m
            => NominalDiffTime
            -> Key
            -> m (Maybe Lock)
acquireLock = j2 AcquireLock

renewLock :: Monad_ Lock_ m
          => NominalDiffTime
          -> Lock
          -> m (Maybe Lock)
renewLock = j2 RenewLock

releaseLock :: Monad_ Lock_ m
            => Lock
            -> m ()
releaseLock = j1 ReleaseLock
```

`j0`, `j1`, and `j2` are combinators for implementing operations
conveniently

```haskell
j0 op = interpret (op id)
j1 op a = interpret (op id a)
j2 op a b = interpret (op id a b)
```

Or we could use `j0 GetCurrentTime`, `j2 AcquireLock` etc. directly,
but that looks rather bizarre.

## Interoperating with `Free` for free

The benefit we get is automatic interaction with free monads.  The
functor of operations that we defined is exactly the base functor of
the corresponding free monad.  There is a function `freely` which
interprets `Free f` in the MTL style `Monad_ f m`

```haskell
freely :: (Functor f, Monad m, Monad_ f m)
       => Free f a -> m a
freely f = foldFree interpret f
```

The inverse to `freely` is `liftF` (which is also the `interpret` of
the `Monad_ f (Free f)` instance).

```haskell
instance Functor f => Monad_ f (Free f) where
  interpret = liftF
```

## Performance

I haven't measured the performance of this but with a suitable
combination of inlining plus `RULE`s it feels like it should perform
equally well to the MTL style.

## Conclusion

Implemention of classes of operations in MTL style contains a common
pattern parametrized by a functor.  Using the parametrized style gives
you automatic interchange between MTL style and free monad style so you can
have the best of both worlds for free!

I'm probably not the first person to notice this.  If anyone knows any
prior references please [let me
know](http://web.jaguarpaw.co.uk/~tom/contact/) and I'll add a
reference.
