```.hs
-- WARNING
```

Preview only: do not circulate

# Haskell's missing mutable reference type

Haskell is missing a mutable reference type similar to what Java calls
a *scoped value*, Python a *context variable* and Common Lisp a
*"special" variable*.

## Introducing `IOScopedRef`

The reference type, which we'll call `IOScopedRef`, would be similar
to
[`IORef`](https://www.stackage.org/haddock/lts-24.38/base-4.20.2.0/Data-IORef.html#t:IORef)
in that you would be able to modify it in `IO`.  It would differ from
`IORef` in that modifications to it would only be visible within a
particular scope. Whereas to modify an `IORef` you have

```.hs
modifyIORef :: IORef a -> (a -> a) -> IO ()
```

for `IOScopedRef` you would have

```.hs
modifyIOScopedRef :: IOScopedRef a -> (a -> a) -> (IO r -> IO r)
modifyIOScopedRef ref f body = ...
```

and the effect of `f` on `ref` is visible within `body` only, and no
longer once it has finished executing.  For example, with `IORef` we
observe modifications due to any intervening executed code:

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
`ref` would be reset to whatever it was when entering it. This would
happen regardless of whether the block was left by normal termination
or by abnormal termination (exception).

## What is it for

### The API

First let's write out a basic API for `IOScopedRef`.  Note that, in
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
modifyIORef :: (a -> a) -> IORef a -> IO ()
```

```.hs
type IOScopedRef :: Type -> Type

-- Like ReaderT's runReaderT
withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r

-- Like ReaderT's ask
readIOScopedRef :: IOScopedRef a -> IO a

-- Like ReaderT's local
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> IO r -> IO r
```

### A logging library

Now let's see how to put it to use in a logging library.  The logging
library allows us to use `logMsg` to log `String`s as log messages and
to specify an integer "severity" for each, and to use `modifySeverity`
to locally modify the severity of messages logged within a block.  An
`IOScopedRef` stores the severity adjustment; `logMsg` uses
`readIOScopedRef` to read the current value of the reference;
`modifySeverity` uses `modifyIOScopedRef` to locally modify the
severity adjustment.

#### Logging API

```.hs
type Severity = Int

data Logger = Logger
  { logMsg ::
      Severity ->
      String ->
      IO (),
    modifySeverity ::
      forall a.
      (Severity -> Severity) ->
      IO a ->
      IO a
  }

withStdoutLogger :: Severity -> (Logger -> IO r) -> IO r
withStdoutLogger initial k =
  withIOScopedRef initial $ \ref -> do
    k
      Logger
        { logMsg = \lvl msg -> do
            cur <- readIOScopedRef ref
            putStrLn ("[" ++ show (lvl + cur) ++ "] " ++ msg),
          modifySeverity = \f action ->
            modifyIOScopedRef f ref action
        }
```

#### A simple use of the logging API

For example, let's write some code to get a user and the data of the
user, locally modifying the severity of log messages when the user we
are dealing with is a VIP.

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

Then we see that the message that we are getting the data for a VIP is
logged at a higher severity:

```
> loggerExample
[1] Getting user
[1] Is VIP: True
[10] Getting data
[0] Done
```

### A naive implementation

Since `IOScopedRef` is a mutable reference, we might try to implement
it in terms of `IORef`, as follows.

```.hs
newtype IOScopedRef a = MkIOScopedRef (Data.IORef.IORef a)

withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r
withIOScopedRef a k = do
  ref <- Data.IORef.newIORef a
  k (MkIOScopedRef ref)

readIOScopedRef :: IOScopedRef a -> IO a
readIOScopedRef (MkIOScopedRef ref) = Data.IORef.readIORef ref

-- WARNING: This does not have the desired behavior!
modifyIOScopedRefBad :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRefBad f (MkIOScopedRef ref) k = do
  orig <- Data.IORef.readIORef ref
  Data.IORef.modifyIORef ref f
  r <- k
  Data.IORef.writeIORef ref orig
  pure r
```

This does not have the desired behavior because it is not exception
safe.  For example, this client code

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
          Control.Exception.throw Exception
          getData user
      )
  writeData d
  logMsg logger 0 "Done"
```

gives this output, which is wrong because the "Get exception" and
"Done" log messages are logged at 10 more than the severity they
should be.

```
-- > loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [11] Got exception
-- [10] Done
```

### Using `bracket`

Why did this happen? Because when the exception was thrown, execution
skipped past the `Data.IORef.writeIORef ref orig` line that is
supposed to restore the original value.  Haskell already has a way of
fixing that though: `bracket`. It allows us to define a cleanup action
that runs regardless of whether the body exited normally or abnormally
(via exception).  Let's see if this is good:

```.hs
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRef f (MkIOScopedRef ref) k =
  Control.Exception.bracket
    (Data.IORef.readIORef ref)
    -- Cleanup action
    (Data.IORef.writeIORef ref)
    -- Body
    ( \orig -> do
        Data.IORef.modifyIORef ref f
        k
    )
```

### Concurrency

This fixes the exception problem and causes `loggerExampleException`
to give the expected output:

```
-- > loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [1] Got exception
-- [0] Done
```

But it has a different problem: it is not thread safe.  Consider
another function which uses the same `Logger` concurrently in two
threads:

```.hs
logggerExampleConcurrently :: IO ()
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

The output is again wrong.  The modification to reduce the severity
for the background processing thread affected the concurrent thread
getting the VIP data!

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


## Comparisons to other languages

* *Haskell*

  * **type**:  `IOScopedRef`
  * **create**: `withIOScopedRef init (\ref -> ...)`
  * **modify**: `modifyIOScopedRef ref f action`
  * **read**: `readIOScopedRef ref`
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

* *Common Lisp*
  * **type**: special (dynamic) variable defined via `defvar` (by
    convention the variable name starts and ends with `*`)
  * **create**: `defvar *v* init`
  * **modify**: `(let ((*v* x)) ...)`
  * **read**: `*v*`
  * **details**: <https://www.lispworks.com/documentation/lcl50/aug/aug-109.html>
