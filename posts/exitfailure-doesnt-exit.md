# `exitFailure` doesn't exit

-- nor do `exitSuccess`, `exitWith` and `die`

Haskell's `base` library contains
[`System.Exit.exitFailure`](https://hackage.haskell.org/package/base-4.20.0.0/docs/System-Exit.html#v:exitWith).
From the name, it sounds as though running it ought to cause your
Haskell program to exit (with a failure exit code).  But it doesn't,
at least not directly.  Instead it throws an exception of type
[`ExitCode`](https://hackage.haskell.org/package/base-4.20.0.0/docs/System-Exit.html#t:ExitCode)
(specifically, `ExitFailure 1`).  Then it is the responsibility of
some exception handler to catch that exception and cause the program
to exit appropriately.  What handler is that?  It is
[`real_handler`](https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.TopHandler.html#real_handler)
in `GHC.Internal.TopHandler`.  It has a special case for `ExitCode`
exceptions:

```.hs
case fromException se of
    Just ExitSuccess     -> exit 0
    Just (ExitFailure n) -> exit n
```

`exit` ultimately calls the runtime system (RTS) function
[`shutdownHaskellAndExit`](https://gitlab.haskell.org/ghc/ghc/-/blob/25edf84977fa15b9911ecbdf614789893ad0e108/rts/RtsStartup.c#L678-687), 
which causes the RTS to exit and thus the program to terminate.

Why then, instead of exiting by throwing an exception, don't we just
expose that `exit` function and call it directly? [The documentation
for
`exitWith`](https://hackage.haskell.org/package/base-4.20.0.0/docs/System-Exit.html#v:exitWith)
explains:

> As an `ExitCode` is an `Exception`, it can be caught using the
> functions of `Control.Exception`. This means that cleanup
> computations added with
> [`bracket`](https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/GHC-Internal-Control-Exception.html#v:bracket)
> (from `Control.Exception`) are also executed properly on `exitWith`.

Using the exception system to trigger program exit allows resources
that were allocated in a `bracket`, but are not managed by the RTS,
such as files on disk, to be cleaned up before program exit.  That's
useful.  But exceptions-for-exit also has strange consequences: if the
`ExitCode` exception is caught then the program won't exit!

If a
[`try`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Control-Exception.html#v:try)
block catches `ExitCode`, and the body ends up calling one of the exit
functions then the program will not exit.  The correct workaround is
to not do that: don't catch `ExitCode` exceptions (and don't catch
`SomeException` either, which is the "catch all" exception).

```.hs
try @ExitCode $ do
    ...
    exitFailure
```

Similarly, if
[`forkIO`](https://www.stackage.org/haddock/lts-22.38/base-4.18.2.1/Control-Concurrent.html#v:forkIO)
spawns a child thread and the child thread ends up calling one of the
exit functions then `forkIO` swallows the exception and the program
will not exit.  The following code misleadingly prints `main:
ExitFailure 1` (as `forkIO` catches, prints, and discards the
`ExitCode` exception) and then ends up looping forever in
`threadDelay`s.  That's probably not what we wanted!  A workaround is
to use the [`async`
package](https://hackage.haskell.org/package/async-2.2.5/docs/Control-Concurrent-Async.html)
rather than `forkIO`.  `async` provides structured concurrency
primitives that are harder to misuse.  In particular, when a thread
forked by `async` terminates with an exception then `async` rethrows
the exception to the parent thread.

```.hs
forkIO $ do
    ...
    exitFailure
forever $
  threadDelay (1000 * 1000)
```

So `exitFailure` doesn't exit, it throws an exception.  The exception
will cause the program exit if it gets to the top of the main thread.
If it doesn't, it won't, so some care is needed!
