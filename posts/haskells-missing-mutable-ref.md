# Haskell's missing mutable reference type

-- Tom Ellis, June 2026

----

This article is part of a collection

* Haskell’s missing mutable reference
  type
* [*A reference implementation of
  `IOScopedRef`*](/posts/ioscopedref-reference-implementation/)
* [*Fork-fragile reader-like operations in Haskell*](/posts/fork-fragile-reader-like-operations/)

----

## Introduction

Haskell is missing a mutable reference type similar to what Java calls
a *scoped value*, Python a *context variable* and Common Lisp a
*"special" variable*. Such a reference can be modified within a
delimited scope and the modification is observable only within that
scope, regardless of whether the reference is shared to child threads
and regardless of whether exceptions are thrown that take execution
back out of the scope of modification.

## Introducing `IOScopedRef`

The reference type, which we'll call `IOScopedRef`, would be similar
to
[`IORef`](https://www.stackage.org/haddock/lts-24.38/base-4.20.2.0/Data-IORef.html#t:IORef)
in that any modifications would be required to take place within `IO`.
It would differ from `IORef` in that modifications to it would only be
observable within a particular scope. As we will see, requiring
modifications to be of this form allows `IOScopedRef` to have
properties that `IORef` does not have.

Specifically, whereas to modify an `IORef` you have

```.hs
modifyIORef :: IORef a -> (a -> a) -> IO ()
```

for `IOScopedRef` you would have

```.hs
modifyIOScopedRef :: IOScopedRef a -> (a -> a) -> (IO r -> IO r)
modifyIOScopedRef ref f body = ...
```

and the effect of `f` on `ref` would be observable within `body` only,
and no longer once it has finished executing.  For example, with
`IORef` we observe modifications due to any intervening executed code:

```.hs
modifyIORef ref (const "hello")
i1 <- readIORef
-- i1 == "hello"
do
   i2 <- modifyIORef ref (const "world")
   -- i2 == "world"
   ... no more modifies ...

i3 <- readIORef ref
-- i3 == "world"
```

but with `IOScopedRef` we do not:

```.hs
modifyIOScopedRef ref (const "hello") $ do
  i1 <- readIOScopedRef ref
  -- i1 == "hello"
  modifyIOScopedRef ref (const "world") $ do
    i2 <- readIOScopedRef ref
    -- i2 == "world"
    ... regardess of any more modfies here ...

  i3 <- readIOScopedRef ref
  -- i3 == "hello"
  ...
```

That is to say, when exiting a `modifyIOScopedRef` block, the value of
`ref` would be reset to what its value was upon entering it. This
would happen regardless of whether the block was left by normal
termination or by abnormal termination (exception).

### Threading

Furthermore, when an `IORef` is shared to child threads, modifications
in one thread can be observed by another thread.  By contrast,
modifications to an `IOScopedRef` in one thread would not be observed
by another thread.  For example, the first thread here might read
`"Second thread"`, depending on the interleaving of the two threads:

```.hs
modifyIORef ref (const "Before threads")

concurrently_
  ( do
      i1 <- readIORef ref
      -- i1 might be "Second thread", depending on how
      -- the threads interleave
      ...
  )
  ( do
      modifyIORef ref (const "Second thread")
      ...
  )
```

but with `IOScopedRef` the threads would be guaranteed to not
interfere, regardless of the interleaving:


```.hs
modifyIOScopedRef ref (const "Before threads") $ do
  concurrently_
    ( do
        i1 <- readIOScopedRef ref
        -- i1 is guaranteed to be "Before threads",
        -- regardless of how the threads interleave
        ...
    )
    ( do
        modifyIOScopedRef ref (const "Second thread") $ do
          ...
    )
```

## Examples using `IOScopedRef`

### The API

First let's write out the basic API for `IORef` and then see the
analogous operations in a basic API for `IOScopedRef`.  Note that, in
the same way that the operations on `IORef` correspond to operations
in the `StateT` monad transformer, operations on `IOScopedRef`
correspond to operations in the `ReaderT` monad transformer.

```.hs
type IORef :: Type -> Type

-- Can be used to write equivalent of StateT's runStateT
newIORef :: a -> IO (IORef a)

-- Like StateT's get
readIORef :: IORef a -> IO a

-- Like StateT's modify
-- (can be used to implement writeIORef)
modifyIORef :: (a -> a) -> IORef a -> IO ()
```

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

If you are thinking *"the `IOScopedRef` API is a restricted form of
the `IORef` API"* then you are right. But `IOScopedRef` *cannot* be
implemented using `IORef`, because the API restrictions allow
`IOScopedRef` to have additional properties that `IORef` does not
have. `IOScopedRef` is a natural counterpart to `IORef` but neither
can be implemented in terms of the other.  `IOScopedRef` is genuinely
a feature missing from Haskell.  Read on to find out more.

### A logging library

Next let's see how to put the `IOScopedRef` API to use in a library,
in this case a logging library.  The logging library allows us to use
`logMsg` to log `String`s as log messages, specifying an integer
"severity" for each, and to use `modifySeverity` to locally modify the
severity of messages logged within a block.  An `IOScopedRef` stores
the severity adjustment; `logMsg` uses `readIOScopedRef` to read the
current value of the reference; `modifySeverity` uses
`modifyIOScopedRef` to locally modify the severity adjustment.

#### Logging API

```.hs
type Severity = Int

data Logger = Logger
  { logMsg ::
      Severity -> String -> IO (),
    modifySeverity ::
      forall a. (Severity -> Severity) -> IO a ->  IO a
  }

withStdoutLogger :: Severity -> (Logger -> IO r) -> IO r
withStdoutLogger initial k =
  withIOScopedRef initial $ \ref -> do
    k
      Logger
        { logMsg = \lvl msg -> do
            adj <- readIOScopedRef ref
            putStrLn ("[" ++ show (lvl + adj) ++ "] " ++ msg),
          modifySeverity = \f action ->
            modifyIOScopedRef f ref action
        }
```

#### A simple use of the logging API

By way of example let's write some code to get a user and the data of
the user, locally modifying the severity of log messages when the user
we are dealing with is a VIP.

```.hs
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

When we are dealing with a VIP the corresponding message is logged at
a higher severity:

```
ghci> loggerExample
[1] Getting user
[1] Is VIP: True
[10] Getting data
[0] Done
```

### A naive implementation

We observed above that the `IOScopeRef` API was a restricted form of
the `IORef` API, but nonetheless that we could not implement it using
`IORef`.  Now let's see why not.  The first thing we might try is to
store the value of the reference in an `IORef`, modifying the `IORef`
when we enter the body and restoring the original value when we leave.

```.hs
newtype IOScopedRef a = MkIOScopedRef (Data.IORef.IORef a)

withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r
withIOScopedRef a body = do
  ref <- Data.IORef.newIORef a
  body (MkIOScopedRef ref)

readIOScopedRef :: IOScopedRef a -> IO a
readIOScopedRef (MkIOScopedRef ref) = Data.IORef.readIORef ref

-- WARNING: This does not have the desired behavior!
modifyIOScopedRefBad :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRefBad f (MkIOScopedRef ref) body = do
  -- Get the original value
  orig <- Data.IORef.readIORef ref
  -- Modify the reference
  Data.IORef.modifyIORef ref f
  -- Run the body
  r <- body
  -- Restore the original value
  Data.IORef.writeIORef ref orig
  -- Return the result of the body
  pure r
```

This implementation will work for `loggerExample` above but it does
not have the desired behavior in all cases: it is not exception safe.
For example, the following client code which throws an exception in
the body

<a id="loggerExampleException">

```.hs
loggerExampleException :: IO ()
loggerExampleException = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  d <-
    Control.Exception.handle
      (\Exception -> logMsg logger 1 "Got exception")
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          Control.Exception.throwIO Exception
          getData user
      )
  writeData d
  logMsg logger 0 "Done"
```

gives the following output, which is wrong because the "Getting data"
and "Done" log messages are logged at 10 more than the severity they
should be.

```
-- > loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [11] Got exception
-- [10] Done
```

The severity adjustment should have been reset to `0` when we left
`modifySeverity` block.

### Using `bracket`

Why did this happen? Because when the exception was thrown, execution
skipped past the `Data.IORef.writeIORef ref orig` line that is
supposed to restore the original value, `0`.  Haskell already has a
way of tackling that problem:
[`bracket`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html#v:bracket). It
allows us to define a cleanup action that runs regardless of whether
the body exited normally or abnormally (via exception). We might try
to use it as follows.  We will see that it fixes the exception problem
yet suffers from another problem.

```.hs
-- WARNING: This does not have the desired behavior!
modifyIOScopedRefStillBad ::
  (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRefStillBad f (MkIOScopedRef ref) body =
  Control.Exception.bracket
    (Data.IORef.readIORef ref)
    -- Cleanup action
    (Data.IORef.writeIORef ref)
    -- Body
    ( \_ -> do
        Data.IORef.modifyIORef ref f
        body
    )
```

Under that implementation `loggerExampleException` gives the desired
output.  The severity adjustment is reset to `0` on leaving the
`modifySeverity` block, even though that was via exception:

```
-- > loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [1] Got exception
-- [0] Done
```

### Concurrency

But there is another problem: it is not thread safe.  Consider a
function which uses the same `Logger` concurrently in two child
threads:

```.hs
loggerExampleConcurrently :: IO ()
loggerExampleConcurrently = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id
  (d, ()) <-
    Control.Concurrent.Async.concurrently
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          getData user
      )
      ( -- Do some unimportant background processing
        modifySeverity logger (subtract 100) $ do
          Control.Concurrent.threadDelay 1000
      )
  writeData d
  logMsg logger 0 "Done"
```

The output is wrong in a different way.  The modification to reduce
the severity for the background processing thread affected the
concurrent thread getting the VIP data!

```
-- > loggerExampleConcurrently
-- [1] Getting user
-- [1] Is VIP: True
-- [-90] Getting data
-- [0] Done
```

We want the output to be the same as our original `loggerExample`,
that is:

```
-- > loggerExampleConcurrently
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [0] Done
```

To achieve this, thread creation primitives must behave in a way that
ensures that local modifications to `IOScopedRef`s within threads are
not observable to concurrently executing threads.  Because such
modifications are only observable locally, this means that the
interaction between the semantics of thread creation and of
`IOScopedRef` are good. In particular, in a function call like

```.hs
myFunction
   ( \arg -> ...
       modifyIOScopedRef ref f $ do
         ... body ...
   )
```

the values of `ref` observed in `body` would not depend on whether
`myFunction` chooses to execute its argument in a new thread.

## Implementing `IOScopedRef`

I believe that `IOScopedRef` cannot be implemented using GHC's
existing mutable reference primitives (such as `IORef`, `MVar`,
`TVar`); it requires something genuinely new.  There is a GHC proposal
to add a similar feature ([scoped thread-locals
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/751))
and I proposed the `IOScopedRef` API in particular in a [discussion
comment](https://github.com/ghc-proposals/ghc-proposals/pull/751#issuecomment-4295909948).

## Next article

Here ends the introduction to `IOScopedRef`.  The article series
continues with *"[A reference implementation of
`IOScopedRef`](/posts/ioscopedref-reference-implementation/)"*.

----

## Appendix

### Partial implementation in Haskell

The [`context`
package](https://hackage-content.haskell.org/package/context) provides
a partial implementation of a type equivalent to `IOScopedRef`. Its
API is as follows

```.hs
-- Equivalent to `IOScopedRef ctx`
type Store ctx

-- Equivalent to `withIOScopedRef`
withNonEmptyStore :: MonadIO m => ctx -> (Store ctx -> m a) -> m a

-- Equivalent to `readIOScopedRef`
mine :: MonadIO m => Store ctx -> m ctx

-- Equivalent to `modifyIOScopedRef`
adjust :: MonadIO m => Store ctx -> (ctx -> ctx) -> m a -> m a
```

It cannot serve as a complete implementation of `IOScopedRef` because
a `Store` when read in a child thread created with standard thread
creation primitives does not pick up modifications that were made to
it in the parent thread.  For example:

```.hs
Context.withNonEmptyStore "Hello" $ \store -> do
  x1 <- Context.mine store
  -- "Hello", as desired
  putStrLn x1

  Context.adjust store (++ " world") $ do
    x2 <- Context.mine store
    -- "Hello world", as desired
    putStrLn x2

    Control.Concurrent.Async.concurrently_
      (pure ())
      ( do
          x3 <- Context.mine store
          -- "Hello", but it should be "Hello world"
          putStrLn x3
      )
```

As a convenience, `context` provides its own thread-creation
operations in
[`Context.Concurrent`](https://hackage-content.haskell.org/package/context-0.2.1.1/docs/Context-Concurrent.html)
which are `Store`-aware and propagate the desired value of all
`Store`s in scope to child threads. For more details on this sort of
behaviour see [*Fork-fragile reader-like operations in
Haskell*](/posts/fork-fragile-reader-like-operations/).

### Similar features in other languages

At least Java, Python and Common Lisp have primitives that have
similar behavior to `IOScopedRef`, *scoped values*, *context
variables* and *"special" variables* respectively.  The following is a
very basic survey of the basic operations available for each
language's primitive.

* *Proposed for Haskell*

  * **type**:  `IOScopedRef`
  * **create**: `withIOScopedRef "v" (\var -> ...)`
  * **modify**: `modifyIOScopedRef var f ...`
  * **read**: `readIOScopedRef var`
  * **details**: <https://github.com/ghc-proposals/ghc-proposals/pull/751#issuecomment-4295909948>

* *Java*

  * **type**:  `ScopedValue`
  * **create**: `ScopedValue.where(KEY, init)`
  * **modify**: `ScopedValue.where(KEY, x).run(() -> ...)`
  * **read**:  `KEY.get()`
  * **details**:
    <https://docs.oracle.com/en/java/javase/25/core/scoped-values.html>

* *Python*
  * **type**:  `ContextVar`
  * **create**:  `var = ContextVar("v")`
  * **modify**:  `token = var.set(x)` + `try: ... finally: var.reset(token)`
  * **read**: `var.get()`
  * **details**: <https://docs.python.org/3/library/contextvars.html>

* *Racket*
  * **type**: parameter created via `make-parameter`
  * **create**: `(define var (make-parameter "v"))`
  * **modify**: `(parametrize ([var x]) ...)`
  * **read**: `var`
  * **details**: <https://docs.racket-lang.org/reference/parameters.html>

* *Common Lisp*
  * **type**: special (dynamic) variable defined via `defvar` (by
    convention the variable name starts and ends with `*`)
  * **create**: `(defvar *var* "v")`
  * **modify**: `(let ((*var* x)) ...)`
  * **read**: `*var*`
  * **details**: <https://www.lispworks.com/documentation/lcl50/aug/aug-109.html>

### Use cases in other languages

#### Local overriding of numeric precision and error handling

Numpy [uses a
`ContextVar`](https://github.com/numpy/numpy/blob/main/numpy/_core/_ufunc_config.py#L386)
to allow [local overriding of error handling
behaviour](https://numpy.org/doc/stable/reference/generated/numpy.errstate.html),
i.e. what should happen when a numeric operation returns `inf`
(infinity) or `NaN` (not a number).
[`mpmath`](https://mpmath.org/doc/current/basics.html#temporarily-changing-the-precision)
and
[`decimal`](https://docs.python.org/3/library/decimal.html#mitigating-round-off-error-with-increased-precision)
allow [locally overriding the desired numeric
precision](https://docs.python.org/3/library/decimal.html#decimal.localcontext).

#### Local overriding of privilege level

Racket's feature "[security
guards](https://docs.racket-lang.org/reference/securityguards.html)"
uses it "parameter" feature to allow local overriding of security
level.  Although not through a "scoped variable" as such, PostgreSQL
has [a similar
feature](https://www.postgresql.org/docs/current/sql-set-role.html).
