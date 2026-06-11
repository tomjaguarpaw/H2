# Fork-fragile reader-like operations in Haskell

-- Tom Ellis, June 2026

----

This article is part of a collection

* [*Haskell’s missing mutable reference
  type*](/posts/haskells-missing-mutable-ref/)
* [*A reference implementation of
  `IOScopedRef`*](/posts/ioscopedref-reference-implementation/)
* Fork-fragile reader-like operations in Haskell

----

## Introduction

The Haskell ecosystem contains several examples of
"[reader](https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/Control-Monad-Trans-Reader.html)-like"
operations that run in `IO` rather than in a specific "reader-like"
monad.  They necessarily have fragile behaviour when
performed in a forked thread, because Haskell does not yet have
suitable primitives with which to implement such operations robustly.
(For more information see [*Haskell's missing mutable reference
type*](/posts/haskells-missing-mutable-ref).)  This article catalogues
some examples.

## Reader-like operations in `IO`

The reader-like operations in question are, firstly, an operation to
read an ambient state (analogous to [`ReaderT`'s
`ask`](https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/Control-Monad-Trans-Reader.html#v:ask),
and with a type like `IO a`) and, secondly, an operation to locally
modify the ambient state (analogous to [`ReaderT`'s
`local`](https://hackage-content.haskell.org/package/transformers-0.6.3.0/docs/Control-Monad-Trans-Reader.html#v:local),
and with a higher order type like `IO a -> IO a`).  There are two
approaches one can take to implement such operations.

1. Store the ambient state in a mutable reference type, shared between
   all operations that interact with it.  "Asking" for the current
   value of the ambient state amounts to reading the current value of
   the reference; "locally modifying" the ambient state amounts to
   updating the value of the reference when entering the local block
   and restoring the original value when leaving the local block.

This is an example of implementation strategy 1:

```.hs
ambientState :: IORef StateType
ambientState = unsafePerformIO (newIORef initialValue)

ask :: IO StateType
ask = readIORef ambientState

local :: (StateType -> StateType) -> IO r -> IO r
local f body = do
  -- Read the original value of the state
  orig <- readIORef ambientState
  bracket_
    -- Modify the state to its new value
    (modifyIORef ambientState f)
    -- Restore the original value of the state
    (writeIORef ambientState orig)
    body
```

Implementation strategy 1 leads to a particular type of fork-fragility,
which I'll call *"Symptom 1"*.

* *Symptom 1:* concurrent threads can observe the (supposedly) "local"
  modifications performed in other threads.

For example:

```.hs
concurrently_
  ( do
      ...
      local f $ do
        ...
      ...
  )
  ( do
      ...
      -- ask may see modification due
      -- to `local f` in sibling thread
      s <- ask
      ...
  )
```

([*Haskell's missing mutable reference
  type*](/posts/haskells-missing-mutable-ref) gives another example that
  exhibits *Symptom 1*.)

The second approach to implement reader-like operations in `IO` is:

2. Store the ambient state in a mutable reference type, shared between
   all operations that interact with it *within a given thread*.
   "Asking" for the current value of the ambient state amounts to
   reading the current value of *that thread's* reference; "locally
   modifying" the ambient state amounts to updating the value of the
   *that thread's* reference.

This is an example of implementation strategy 2:

```.hs
-- An ambient state *for each thread*
ambientState :: IORef (Map ThreadId StateType)
ambientState = unsafePerformIO (newIORef Map.empty)

ask :: IO StateType
ask = do
  m <- readIORef ambientState
  t <- myThreadId
  -- Look up the value of *this thread's* ambient state
  pure (fromJust (Map.lookup t m))

local :: (StateType -> StateType) -> IO r -> IO r
local f body = do
  -- Read the original value of the state
  m <- readIORef ambientState
  t <- myThreadId
  let orig = fromJust (Map.lookup t m)

  bracket_
    -- Modify the state to its new value,
    -- for *this thread* only
    ( atomicModifyIORef' ambientState $ \m' ->
        (Map.insert t (f orig) m', ())
    )
    -- Restore the original value of the state,
    -- for *this thread* only
    ( atomicModifyIORef' ambientState $ \m' ->
          (Map.insert t orig m', ())
    )
    body
```

Implementation strategy 2 leads to a different type of fork-fragility,
which I'll call *"Symptom 2"*.

* *Symptom 2:* Child threads created through standard thread-creation
  primitives do not automatically inherit the ambient state of the
  parent but rather must reinitialise it somehow (either manually or
  through using library-specific thread-creation primitives).

For example:

```.hs
local g $ do
  forkIO $ do
    -- ask does not see the value
    -- of the state set by g
    s <- ask
    ...
  ...
```

## A potential solution

To implement reader-like operations in `IO` that are subject to
neither symptom, Haskell needs a new feature.  A hypothetical such
feature is described in these articles:

* *[Haskell's missing mutable reference
  type](/posts/haskells-missing-mutable-ref)*
* *[A reference implementation of
  `IOScopedRef`](/posts/ioscopedref-reference-implementation/)*

In the absence of such a feature, reader-like operations in `IO` will
continue to exhibit fork-fragile behaviour.  This article concludes
with a catalogue of some ecosystem examples of reader-like operations
which, except in the special case of masking operations, inevitably
exhibit such behaviour.

## Masking: fork-resilient reader-like operations

[`Control.Exception`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html)
has
[`mask_`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html#v:mask_),
[`uninterruptibleMask_`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html#v:uninterruptibleMask_)
and
[`interruptible`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html#v:interruptible)
(each of type `IO a -> IO a`), reader-like operations which, in
effect, locally modify a value of type
[`MaskingState`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Exception.html#t:MaskingState)
within their body.  Local modifications to the masking state in one
thread are not observed within other threads but the masking state
*is* inherited by child threads, so these operations are *not*
fork-fragile.  How?  They have a special implementation in GHC's RTS.
Unfortunately, that implementation cannot currently be used as a way
to obtain user-defined local states.

## Catalogue of fork-fragile reader-like operations

* `withArgs` and `withProgName` allow the program's arguments and
  program name to be locally overridden.

    * **Package**: `base`
    * **Local-like operations**:
        * [`withArgs :: [String] -> IO a -> IO a`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-Environment.html#v:withArgs)
        * [`withProgName :: String -> IO a -> IO a`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-Environment.html#v:withProgName)
    * **Local state**: Program arguments and program name
    * **Storage**: RTS-wide argv table
    * **Fork-fragile symptom**: Symptom 1

* Internal functions in testing libraries allow `Handle` buffering
  setting to be locally overridden

    * **Packages**: `QuickCheck`, `hspec-core`
    * **Local-like operations**:
        * [`withBuffering :: IO a -> IO
          a`](https://github.com/nick8325/quickcheck/blob/3c2d0d87d4b9f457a586b1cde6d1d3ac03d862bf/src/Test/QuickCheck/Text.hs#L175-L181)
        * [`withLineBuffering :: IO a -> IO
          a`](https://github.com/hspec/hspec/blob/main/hspec-core/src/Test/Hspec/Core/Formatters/Internal.hs#L240-L242)
    * **Local state**: Current buffering mode for standard handles
    * **Storage**: Mutable GHC
      [`Handle`](https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/src/GHC.Internal.IO.Handle.Types.html#Handle)
      state for `stdout` and `stderr`
    * **Fork-fragile symptom**: Symptom 1

* `with-utf8` functions allow `Handle` default encoding setting to be
  locally overridden

    * **Package**: `with-utf8`
    * **Local-like operations**:
        * [`withUtf8 :: MonadIO m => m r -> m r`](https://hackage.haskell.org/package/with-utf8-1.1.0.0/docs/Main-Utf8.html#v:withUtf8)
        * [`withStdTerminalHandles :: MonadIO m => m r -> m r`](https://hackage.haskell.org/package/with-utf8-1.1.0.0/docs/Main-Utf8.html#v:withStdTerminalHandles)
    * **Local state**: Current encoding for standard handles and
      default locale encoding
    * **Storage**: Mutable GHC
      [`Handle`](https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/src/GHC.Internal.IO.Handle.Types.html#Handle)
      state for `stdin`, `stdout` and `stderr`, and GHC's
      program-global locale encoding
    * **Fork-fragile symptom**: Symptom 1

* `context` is a library that allows arbitrary state to be locally overridden

    * **Package**: `context`
    * **Local-like operation**:
        * [`adjust :: MonadIO m => Store ctxt -> (ctx -> ctx) -> m a -> m a`](https://hackage-content.haskell.org/package/context-0.2.1.1/docs/Context.html#v:adjust)
    * **Local state**: User-defined value of type `ctx`
    * **Storage**: `Store` is implemented with an `IORef` containing a
      thread-indexed `Map`
    * **Used by**:
        * [`withThreadContext`](https://hackage-content.haskell.org/package/monad-logger-aeson-0.4.1.6/docs/Control-Monad-Logger-Aeson.html#v:withThreadContext)
           in
           [`monad-logger-aeson`](https://hackage.haskell.org/package/monad-logger-aeson)
           and [`Blammo`](https://hackage.haskell.org/package/Blammo),
           to attach context to log messages
        * [`modifyResponsesWithContext`](https://hackage.haskell.org/package/context-http-client-0.2.0.2/docs/Network-HTTP-Client-Context.html#v:modifyResponsesWithContext)/[`modifyRequestsWithContext`](https://hackage.haskell.org/package/context-http-client-0.2.0.2/docs/Network-HTTP-Client-Context.html#v:modifyRequestsWithContext)
          in
          [`context-http-client`](https://hackage.haskell.org/package/context-http-client-0.2.0.2),
          to apply a modification to all incoming requests or outgoing responses
    * **Fork-fragile symptom**: Symptom 2
    * **Notes**: `Store ctx` is identical to what [`IOScopedRef
      ctx`](/posts/haskells-missing-mutable-ref) would be (with
      `adjust` corresponding to `modifyIOScopedRef` and `use`
      corresponding to `readIOScopedRef`) except that `IOScopedRef`
      would have the desired behaviour around access within a new
      thread whilst `Store` does not (and cannot, with current GHC
      primitives).

* OpenTelemetry allows defining enclosing spans for blocks of code
  for improved observability

    * **Package**: `hs-opentelemetry-api`
    * **Local-like operation**:
        * [`MonadUnliftIO m => inSpan :: ... -> m a -> m a`](https://hackage-content.haskell.org/package/hs-opentelemetry-api-1.0.0.0/docs/OpenTelemetry-Trace-Core.html#v:inSpan)
    * **Local state**: Current span stack and context
    * **Storage**: Per-thread `IORef`
    * **Fork-fragile symptom**: Symptom 2

* Instana SDK allows defining enclosing spans for blocks of code
  for improved observability

    * **Package**: `instana-haskell-trace-sdk`
    * **Local-like operation**:
        * [`withRootEntry :: MonadIO m => InstanaContext -> SpanType
            -> m a -> m
            a`](https://hackage.haskell.org/package/instana-haskell-trace-sdk-0.10.2.0/docs/Instana-SDK-SDK.html#v:withRootEntry)
        * `withEntry`, `withExit`, ...
    * **Local state**: Current tracing span stack
    * **Storage**: [Shared thread-indexed `TVar`](https://github.com/instana/haskell-trace-sdk/blob/02e9960d6358340308a9d5ee0dd7ce70ede86aa7/src/Instana/SDK/Internal/Context.hs#L145)
    * **Fork-fragile symptom**: Symptom 2

* Thread-indexed logging settings allow logging settings to be locally overridden

    * **Package**: `heavy-logger`
    * **Local-like operation**:
        * [`withLoggingIO :: LoggingSettings -> IO a -> IO
            a`](https://hackage.haskell.org/package/heavy-logger-0.3.2.2/docs/System-Log-Heavy-IO.html#v:withLoggingIO)
    * **Local state**: Current logger, backend and log context
    * **Storage**: Global thread-indexed `IORef`
    * **Fork-fragile symptom**: Symptom 2

* Global IO logging settings allow logging settings to be locally overridden

    * **Packages**: `logging`, `simple-logger`, `hslogger`, `nvim-hs`
    * **Local-like operations**:
        * [`withStdoutLogging :: MonadIO m =>
          m a -> m a
          `](https://hackage-content.haskell.org/package/logging-3.0.6/docs/Control-Logging.html#v:withStdoutLogging)
        * [`withStderrLogging :: MonadIO m =>
          m a -> m a
          `](https://hackage-content.haskell.org/package/logging-3.0.6/docs/Control-Logging.html#v:withStderrLogging)
        * [`withFileLogging :: MonadIO m =>
          FilePath -> m a -> m a
          `](https://hackage-content.haskell.org/package/logging-3.0.6/docs/Control-Logging.html#v:withFileLogging)
        * [`withGlobalLogging :: LogConfig -> IO a -> IO
          a`](https://hackage.haskell.org/package/simple-logger-0.1.1/docs/Control-Logger-Simple.html#v:withGlobalLogging)
        * [`withLogger :: FilePath -> Priority -> IO a -> IO a `](https://hackage-content.haskell.org/package/nvim-hs-2.3.2.4/docs/Neovim-Log.html#v:withLogger)  
    * **Local state**: Current global logger set, log level, time
      format etc.
    * **Storage**: Top-level mutable state
    * **Fork-fragile symptom**: Symptom 1
    * **Notes**: These reader-like operations are "supposed to" be
      only used at the top level of your program, but nothing enforces
      that

* String-indexed storage allows arbitrary data to be stored under a
  string key, and locally overridden

    * **Package**: `io-storage`
    * **Local-like operation**:
        * [`withStore :: String -> IO a -> IO a`](https://hackage.haskell.org/package/io-storage-0.3/docs/System-IO-Storage.html#withStore)
    * **Local state**: Current named dynamic store
    * **Storage**: Top-level `IORef`
    * **Fork-fragile symptom**: Symptom 1
    * **Notes**: Formerly used by [`hledger-web`](https://github.com/simonmichael/hledger/blob/c6a85c4b88d7b5cad074388bcee19a10c472bff6/hledger-web/Hledger/Web/Main.hs#L85)
