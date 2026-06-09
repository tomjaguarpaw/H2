# A reference implementation of `IOScopedRef`

-- Tom Ellis, June 2026

----

This article is part of a collection

* [*Haskell’s missing mutable reference
  type*](/posts/haskells-missing-mutable-ref/)
* A reference implementation of
  `IOScopedRef`
* [*Fork-fragile reader-like operations in Haskell*](/posts/fork-fragile-reader-like-operations/)

----

## Introduction

In a previous article *"[Haskell's missing mutable reference
type](../haskells-missing-mutable-ref/)*" I presented the API of
"`IOScopedRef`", a potential new mutable reference type for Haskell,
and described how it should behave: an `IOScopedRef` should be a
reference that can be locally modified, and the local modification
should behave well with respect to exceptions and threading.  I also
demonstrated the failure of an attempt to implement `IOScopedRef` in
terms of `IORef`. This article contains a reference implementation
that succeeds.

## The API

Let's remind ourselves of the API of `IOScopedRef`:

```.hs
type IOScopedRef :: Type -> Type

-- Like ReaderT's runReaderT
-- (no direct equivalent of newIORef)
withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r

-- Like ReaderT's ask
readIOScopedRef :: IOScopedRef a -> IO a

-- Like ReaderT's local
-- (there is no direct equivalent of writeIORef)
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> IO r -> IO r
```

The comments about the behaviour of `IOScopedRef` being analogous to
`ReaderT` already contain a clue to how we could implement it, so
let's look at the API of `ReaderT`:

```.hs
type ReaderT :: Type -> (Type -> Type) -> Type

-- Like IOScopedRef's withIOScopedRef
runReaderT :: ReaderT a m r -> a -> m r

-- Like IOScopedRef's readIOScopedRef
ask :: ReaderT a m a

-- Like IOScopedRef's modifyIOScopedRef
local :: (a -> a) -> ReaderT a m r -> ReaderT a m r
```

Does that help? Not immediately, because although `ReaderT` allows us
to run a computation in a "scope with local modification" the
modification can only be to one value of type `a`, whereas the
`IOScopedRef` API allows us to hold references to any number of `a`s,
and indeed references to any number of arbitrary types!

## `Vault`

But what if we chose the `a` in `ReaderT a` to be a type which
*itself* can contain any number of references to arbitrary types?
That sounds like a mysterious type indeed, but in the world of Haskell
many mysterious things exist and such a type is one of them: it's
called
[`Vault`](https://hackage.haskell.org/package/vault-0.3.1.5/docs/Data-Vault-Strict.html#t:Vault)
and comes from the package
[`vault`](https://hackage.haskell.org/package/vault).  These are the
elements of its API that will be needed for our purposes:

```.hs
type Vault :: Type

type Key :: Type -> Type

empty :: Vault

newKey :: IO (Key a)

insert :: Key a -> a -> Vault -> Vault

lookup :: Key a -> Vault -> Maybe a

adjust :: (a -> a) -> Key a -> Vault -> Vault
```

A `Vault` is a data structure that implements a "finite mapping" or
"associative container" similar to `Data.Map.Map k v`.  The latter
maps keys of type `k` to values of type `v`.  `Vault` differs from a
`Map` in two important ways. Firstly, the keys of a `Vault` are of an
abstract type `Key a`, inhabitants of which can only be created
through `newKey`. Secondly, if there is a value in a `Vault`
corresponding to a key of type `Key a` then that value is of type `a`.
That is, the type of a value stored in a `Vault` depends on the type
of the key at which it is stored, a property infrequently seen in
associative container types.

The behaviour of the `Vault` API is that `newKey` creates a new key,
and that `empty`, `insert`, `lookup` and `adjust` create, query and
modify a `Vault` in the way the functions of the same names do for
`Data.Map.Map`.  (`Vault` also allows the value corresponding to a key
to be deleted, but we won't use that functionality in this article.)
Here's an example of using these `vault` API elements:

```.hs
vaultExample :: IO ()
vaultExample = do
  -- Create an empty vault
  let v0 = Vault.empty

  -- Create a key
  k0 <- Vault.newKey

  -- Look up key k0 in the vault
  let r1 = Vault.lookup k0 v0
  -- We haven't inserted anything into the vault at key k0 so the
  -- result is
  --
  -- Nothing
  print r1

  -- Insert something into the vault at key k0
  let v1 = Vault.insert k0 "Hello" v0

  -- Look up key k1 in the vault
  let r2 = Vault.lookup k0 v1
  -- Just "Hello"
  print r2

  -- Create another key
  k1 <- Vault.newKey

  -- Look up key k1 in the vault
  let r3 = Vault.lookup k1 v1
  -- We haven't inserted anything into the vault at key k1 so the
  -- result is
  --
  -- Nothing
  print r3

  -- Insert something into the vault at key k1
  let v2 = Vault.insert k1 True v1
  -- Look up each key in the vault. The result is
  --
  -- (Just "Hello", Just True)
  let r4 = (Vault.lookup k0 v2, Vault.lookup k1 v2)
  print r4

  -- Adjust (modify) the value stored at key k0
  let v3 = Vault.adjust (++ " world") k0 v2
  -- We get the modified value at k0 and the unchanged value at k1
  --
  -- (Just "Hello world",Just True)
  let r5 = (Vault.lookup k0 v3, Vault.lookup k1 v3)
  print r5
```

## `IOScopedRef` in terms of `Vault`

Using a `Vault` we can implement `IOScopedRef` as an newtype wrapper
around `Key`, and implement its operations in a monad "`NewIO`" which
provides reader-like access to a `Vault` and supports `IO` actions.

```.hs
-- (The constructor is to be kept hidden)
newtype IOScopedRef a = MkIOScopedRef (Vault.Key a)

-- (The constructor is to be kept hidden)
newtype NewIO a = MkNewIO {unNewIO :: ReaderT Vault.Vault IO a}
  deriving newtype (Functor, Applicative, Monad)

-- IOScopedRefs are introduced by withIOScopefRef, which uses
-- Vault.newKey and Vault.insert to insert the initial value of the
-- IOScopedRef into the Vault
withIOScopedRef :: a -> (IOScopedRef a -> NewIO r) -> NewIO r
withIOScopedRef a body = MkNewIO $ do
  key <- Control.Monad.Trans.lift Vault.newKey
  Reader.local (Vault.insert key a) $ do
    unNewIO (body (MkIOScopedRef key))

-- When reading the IOScopedRef we ask for the vault and lookup the
-- key in it
readIOScopedRef :: IOScopedRef a -> NewIO a
readIOScopedRef (MkIOScopedRef key) = MkNewIO $ do
  vault <- Reader.ask
  case Vault.lookup key vault of
    Nothing -> error "IOScopedRef escaped its scope"
    Just a -> pure a

-- When modifying the IOScopedRef we create a new local scope in
-- ReaderT in which we work with a modification of the Vault
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> NewIO r -> NewIO r
modifyIOScopedRef f (MkIOScopedRef key) (MkNewIO body) = MkNewIO $
  Reader.local (Vault.adjust f key) body
```

According to this attempt a combination of `ReaderT` and `Vault` is
enough to implement `IOScopedRef`!  `NewIO` is identical to `IO`
except that it also supports `IOScopedRef`.  Let's see how the attempt
works in practice.

### Usage

To use `NewIO` with the existing Haskell ecosystem we can derive
instances of `MonadIO` and
[`MonadUnliftIO`](https://hackage-content.haskell.org/package/unliftio-core-0.2.1.0/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO),
the latter of which allows us to use the
[`unliftio`](https://hackage-content.haskell.org/package/unliftio)
package.  Therefore we extend our `deriving` clause to:

```.hs
deriving newtype
  (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
```

And we can define `runNewIO` in order to conveniently run a `NewIO`
computation.

```.hs
runNewIO :: NewIO a -> IO a
runNewIO (MkNewIO rio) = Reader.runReaderT rio Vault.empty
```

Then [the logger API from *"Haskell's missing mutable reference
type"*](../haskells-missing-mutable-ref#logging-api) becomes the
following, by replacing occurrences of `IO` with `NewIO`.  (The
implementation in terms of the `IOScopedRef` API does not change.)

```.hs
data Logger = Logger
  { logMsg ::
      Severity -> String -> NewIO (),
    modifySeverity ::
      forall a. (Severity -> Severity) -> NewIO a -> NewIO a
  }

withStdoutLogger :: Severity -> (Logger -> NewIO r) -> NewIO r
```

#### Simple example, and abnormal termination

The most simple example from the previous article
([`loggerExample`](../haskells-missing-mutable-ref#a-simple-use-of-the-logging-api))
works as desired.

```.hs
-- ghci> loggerExample
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [0] Done
loggerExample :: IO ()
loggerExample = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id
  d <- modifySeverity logger modification $ do
    logMsg logger 0 "Getting data"
    getData user
  writeData d
  logMsg logger 0 "Done"
```

The example that used exceptions
([`loggerExampleException`](../haskells-missing-mutable-ref#loggerExampleException))
also works as desired.  When the body terminates abnormally (via
exception) the original value of the `IOScopedRef` is restored (as it
did under [our earlier implementation using `IORef` and
`bracket`](../haskells-missing-mutable-ref#a-naive-implementation)):

```.hs
-- ghci> runNewIO loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [1] Got exception
-- [0] Done
loggerExampleException :: NewIO ()
loggerExampleException = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  d <-
    UnliftIO.Exception.handle
      (\Exception -> logMsg logger 1 "Got exception")
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          UnliftIO.Exception.throwIO Exception
          getData user
      )
  writeData d
  logMsg logger 0 "Done"
```

#### Concurrency

Even better, the example that uses concurrency
([`loggerExampleConcurrently`](../haskells-missing-mutable-ref#concurrency))
also works as desired, which is something that we could not achieve
with an `IORef`-based implementation:

```.hs
-- ghci> runNewIO loggerExampleConcurrently
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [0] Done
loggerExampleConcurrently :: NewIO ()
loggerExampleConcurrently = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  (d, ()) <-
    UnliftIO.Async.concurrently
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          getData user
      )
      ( -- Do some unimportant background processing
        modifySeverity logger (subtract 100) $ do
          UnliftIO.Concurrent.threadDelay 1000
      )
  writeData d
  logMsg logger 0 "Done"
```

Why did the attempt in terms of `Vault` succeed where the attempt in
terms of `IORef` failed?  Because when we fork child threads using
`UnliftIO.Async.concurrently`, the in-scope `Vault` is passed to each
of them, implicitly by the `MonadUnliftIO` instance of `ReaderT a IO`.
The `Vault` is immutable, and thus nothing one thread does with it can
affect the behaviour of any other thread.  By contrast, `IORef` is
mutable so when we tried to implement `IOScopedRef` in terms of one,
changes to it could be observed across sibling threads.

### `IOScopedRef`s can escape their scope

`IOScopedRef`s come with a notable caveat: they can escape their
scope. The reference implementation can shed some light on this
phenomenon.  Recall that `withIOScopedRef` has the following type, and
that there is no "`newIOScopedRef`" for creating an `IOScopedRef`
directly.

```.hs
withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r

-- This equivalent to newIORef does not exist
newIOScopedRef :: a -> IOScopedRef a
```

We might attempt to implement `newIOScopedRef` as follows

```.hs
-- This does not work!
newIOScopedRef :: a -> NewIO (IOScopedRef a)
newIOScopedRef = withIOScopedRef () pure
```

but if we do, things go wrong:

```.hs
-- ghci> escape
-- *** Exception: IOScopedRef value not in scope
escape :: NewIO ()
escape = do
  ref <- newIOScopedRef ()
  readIOScopedRef ref
```

Why?  In terms of our reference implementation we would say "because
outside the scope of the `Reader.local` that occurs in the definition
of `withIOScopedRef`, the `Vault` is what it was before we entered the
body". In this case, it does not contain the new key allocated in
`withIOScopedRef`.  When we come to `readIOScopedRef` our
`Vault.lookup` returns `Nothing`, so we have no choice but to error
out.

Let's look at an example which uses threads.  In `badShare` below we
pass an `IOScopedRef` created in a thread to a sibling thread, via an
`MVar`.  The sibling thread is not within the scope of the reference
and `readIOScopedRef` fails.  Why?  Again in terms of our reference
implementation "because the `IOScopedRef` was added to the `Vault` in
the reader environment of the first thread and therefore has no entry
in the `Vault` in the second thread".

```.hs
-- ghci> runNewIO badShare
-- *** Exception: IOScopedRef value not in scope
badShare :: NewIO ((), String)
badShare = do
  mvar <- UnliftIO.MVar.newEmptyMVar
  UnliftIO.Async.concurrently
    ( withIOScopedRef "Hello" $ \ref -> do
        UnliftIO.MVar.putMVar mvar ref
        UnliftIO.Concurrent.threadDelay 1000000000
    )
    ( do
        ref' <- UnliftIO.MVar.takeMVar mvar
        readIOScopedRef ref'
    )
```

By contrast, if both threads run in the scope of an `IOScopedRef` then
it is fine to pass that `IOScopedRef` between them.

```.hs
-- ghci> runNewIO goodShare
-- ((),"Hello")
goodShare :: NewIO ((), String)
goodShare = do
  mvar <- UnliftIO.MVar.newEmptyMVar
  withIOScopedRef "Hello" $ \ref -> do
    UnliftIO.Async.concurrently
      (UnliftIO.MVar.putMVar mvar ref)
      ( do
          ref' <- UnliftIO.MVar.takeMVar mvar
          readIOScopedRef ref'
      )
```

Now, the problem of `IOScopedRef`s going out of scope is not a
weakness particular to this reference implementation: it is a
requirement of any implementation, if we are to satisfy other
desirable properties of `IOScopedRef`.  Regardless of how we implement
currency primitives it does not seem possible to assign coherent
semantics to `IOScopedRef`s shared between threads.


Let's see an example.  In `confusingShare` below, if `readIOScopedRef
ref1` does not error out then what should be the value of `a`?  It
must be `"Hello"`. Then by the same logic the value of `b` must be
`"Bye"`.  But can the same reasoning hold in `straightforwardShare`?
It passes around the reference in exactly the same way as
`confusingShare` and differs only because the body of each thread is
in the scope of the creation of the `IOScopedRef`. According to our
design goal that threads interacting with a shared `IOScopedRef`
should not interfere with each other (exemplified by
`loggerExampleConcurrently` above) `b` must be `"Hello"` not `"Bye"`!

```.hs
confusingShare = do
  mvar <- newEmptyMVar
  concurrently_
    ( withIOScopedRef "Hello" $ \ref0 -> do
        putMVar mvar ref0
        ref2 <- takeMVar mvar
        -- If readIOScopedRef succeeds, b ought to be "Bye"
        b <- readIOScopedRef ref2
        threadDelay d
    )
    ( do
        ref1 <- takeMVar mvar
        -- If readIOScopedRef succeeds, a ought to be "Hello"
        a <- readIOScopedRef ref1
        modifyIOScopedRef (const "Bye") ref1 $ do
          putMVar mvar ref1
          threadDelay d
    )

straightforwardShare = do
  withIOScopedRef "Hello" $ \ref0 -> do
    mvar <- newEmptyMVar
    concurrently_
      ( do
          putMVar mvar ref0
          ref2 <- takeMVar mvar
          -- b is "Hello"
          b <- readIOScopedRef ref2
          threadDelay d
      )
      ( do
          ref1 <- takeMVar mvar
          -- a is "Hello"
          a <- readIOScopedRef ref1
          modifyIOScopedRef (const "Bye") ref1 $ do
            putMVar mvar ref1
            threadDelay d
      )

d :: Int
d = ...
```

Therefore, if we desire `IOScopedRef` to satisfy two properties --
firstly that interactions with `IOScopedRef` in concurrent threads do
not interfere with each other (which is property of `IOScopedRef` that
fundamentally distinguishes it from `IORef`) and secondly that the
behaviour of an `IOScopedRef` is independent of subtle operational
details like which thread it was created in -- then we are forced to
conclude that `readIOScopedRef` must fail when called on an argument
that has escaped its scope.

#### Preventing escape using the type system

Even though the consequences of a run time error are less severe than
the consequences of a subtle concurrency bug, it would not be pleasant
to have to keep track of scope when using `IOScopedRef`
concurrently. Can the type system help?

One thing we might try is defining a "checked" version of
`readIOScopedRef` like this:

```.hs
readIOScopedRefMaybe :: IOScopedRef a -> IO (Maybe a)
```

But that doesn't help much.  Users will only attempt to read an
`IOScopedRef` when they believe it to be in scope and so will probably
just end up defining an unchecked version in terms of the checked version:


```.hs
readIOScopedRefUnchecked :: IOScopedRef a -> IO (Maybe a)
readIOScopedRefUnchecked = fmap fromJust . readIOScopedRefMaybe
```

What we want is to be statically prevented from reading an
`IOScopedRef` when it is not in scope.  Is there a way the type system
can help with that?  Yes, there is. I won't elaborate in this article,
but [Bluefin](https://hackage-content.haskell.org/package/bluefin), my
Haskell effect system built on capabilities, explicitly tracks scope
in the type system.  [The `Ask`
capability](https://hackage-content.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin-Capability-Ask.html)
is Bluefin's equivalent of `IOScopedRef`, and Bluefin's scope tracking
ensures that such a capability cannot escape its scope.

(If `IOScopedRef` were implemented in GHC, Bluefin would switch its
implementation of `Ask` to use `IOScopedRef`. A robust implementation
of `Ask`, i.e. one that that both respects concurrency in the way
exemplified by `loggerExampleConcurrently` *and* interoperates with
existing higher order `IO` code, is not currently possible. The
introduction of `IOScopedRef` would resolve that problem.)

### Replacing `IO`

Our original design goal was for `IOScopedRef` to operate in
`IO`. Instead, we have implemented it in some new monad called
"`NewIO`".  How could we bridge that gap? In principle we could take
Haskell's existing definition of `IO`, rename it `OldIO` and define

```.hs
newtype NewIO a = MkNewIO (ReaderT Vault.Vault OldIO a)

newtype IO a = MkIO (NewIO a)
```

and we'd have support for `IOScopedRef` in `IO`!  Is that a plausible
approach?  There are two reasons why it might not be.  Firstly,
[`GHC.Base`
exposes](https://www.stackage.org/haddock/lts-24.42/base-4.20.2.0/GHC-Base.html#v:IO)
the implementation of `IO`, specifically

```.hs
newtype IO = IO (State# RealWorld -> (# State# RealWorld, a #))
```

Therefore, some existing code would break if we changed the definition
of `IO` to

```.hs
newtype IO a = MkIO (NewIO a)

newtype NewIO a = MkNewIO (ReaderT Vault.Vault OldIO a)

newtype OldIO =
  MkOldIO (State# RealWorld -> (# State# RealWorld, a #))
```

Secondly, the implementation in terms of `ReaderT Vault` may not be
the most efficient possible.  It likely imposes an efficiency penalty
on users who do not use `IOScopedRef` at all.  An implementation in
GHC's RTS may overcome that problem.

#### Why implement `IOScopedRef` in `IO` anyway?

But now that we have an implementation of `IOScopedRef` do we even
care that it's not in `IO`?  Can't we just provide it as a library and
be happy with that?  I believe that we *should* insist that
`IOScopedRef` works directly in `IO`, with no wrappers.  Why?  Because
Haskell users need a "lowest common denominator" to program against,
and whatever that is, we should call it `IO`.

To see why, consider the following hypothetical library function,
`runQuery`, which uses `IO` in a "higher order" manner, that is, it
has an argument which is a function which returns `IO`.  The behaviour
of `runQuery` is to run a database query whilst enclosing separate
parts of its implementation in labelled blocks, to support logging,
observability and performance measurement.

```.hs
runQuery :: Query -> (forall a. String -> IO a -> IO a) -> IO Result
runQuery query label = do
  (conn, preparedQuery) <-
    concurrently
      ( label "obtain connection" $ do
          getConnection
      )
      ( label "preparing query" $ do
          prepare query
      )
  label "running query" $ do
    runPrepared conn preparedQuery
```

`runQuery` is implemented using threads so if we want to use `label`
for logging in a similar way to our earlier logging example then we
need to have something like `IOScopedRef`.  If `IOScopedRun` runs in
`NewIO` rather than `IO` then we can't.  `runQuery` doesn't accept an
argument that returns `NewIO`.  What are our options?

* *Rewrite `runQuery` to use `NewIO`*

  We might be able to convert libraries *we* control to `NewIO`; we
  might be able to persuade some third party maintainers to convert
  too.  But even if we could convert *all* libraries to `NewIO`, what
  are we going to do if someone comes up with yet *another* monad that
  supports yet *another* feature?  Rewrite everything again?  Library
  authors want to program against an interface that works once and for
  all. We need to make a choice of monad that won't impose further
  changes on library authors in the future, a "lowest common
  denominator".

* *Rewrite `runQuery` to use `MonadUnliftIO m`*

  `MonadUnliftIO` is a general interface that supports everything that
  we would want to do with `NewIO` and anything we are likely to want
  to do with a hypothetical new monad supporting any hypothetical new
  feature.  Once `runQuery` is written using it, it won't require
  further changes.  So `MonadUnliftIO` is a "lowest common
  denominator".  Unfortunately, using a type class for this purpose
  hurts performance, as explained by Alexis King in *[Effects for
  Less](https://www.youtube.com/watch?v=0jI-AlWEwYI)*, because some
  optimizations are unavailable due to `m`'s bind operation being
  unknown.  Therefore `MonadUnliftIO` is unsatisfactory.

* *Rewrite `runQuery` to use `ReaderT r IO a`*

  Writing `runQuery` in terms of `ReaderT` would allow us to support
  `IOScopedRef` (`NewIO` was just a newtype wrapper over `ReaderT`)
  and it has a known bind, so avoids the performance problems of
  `MonadUnliftIO`.  We could even mix `IOScopedRef` effects (which
  need to read `Vault`) with other effects by tupling up their
  environments (as in `ReaderT (Vault, OtherEnv) IO`).  So `ReaderT`
  is possibly a "lowest common denominator", as long as all new
  features won't require anything other than a reader
  environment. `ReaderT` used that way would become a rudimentary
  effect system.  Some questions remain unresolved: can we,
  practically, rewrite all existing code to this form, and is there a
  performance penalty?

* *Put `IOScopedRef` in `IO`*

  This is by far the simplest option. We accept `IO` as the lowest
  common denominator, where we put all new features that are not pure,
  all existing code can remain the same, all future code can also
  target this interface.

There is one remaining question before choosing `IO` as the
destination for `IOScopedRef`:

*"Doesn't this risk making `IO` too big?*

It makes `IO` bigger, and implies that it will become yet bigger with
time as new features are added, but personally I don't see that as a
problem.  The large and wild domain of `IO` can be tamed by "analytic"
effect systems (such as
[`effectful`](https://hackage.haskell.org/package/effectful) and
[Bluefin](https://hackage.haskell.org/package/bluefin)), which carve
`IO` into smaller well-behaved pieces.  Making `IO` "bigger" by
adopting `IOScopedRef` into it would have no impact on a user of an
analytic effect system.  Conversely, new features in `IO` can be
adopted by analytic effect systems, which can restrict their use to
well-defined parts of a program.

----

## Notes

By using `Vault` in a `ReaderT` we have obtained a mutable reference
type, `IOScopedRef`, just a newtype around `Key`, that itself behaves
in a "reader-like" way. This mirrors what van der Ploeg, Claessen and
Buiras achieved in [*The Key monad: type-safe unconstrained dynamic
typing*]( https://dl.acm.org/doi/10.1145/3241625.2976008) where they
used a type isomorphic to `Vault` (which they call `KeyMap`) in a
`StateT` and obtained a mutable reference type, `STRef`, just a
newtype around their `Key`, which itself behaves in a "state-like"
way.

## Acknowledgements

Thanks to Simon Peyton Jones for helpful discussion and suggestions.
