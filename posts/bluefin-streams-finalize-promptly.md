# Bluefin streams finalize promptly

-- pipes and conduit streams don't

The [pipes](https://hackage.haskell.org/package/pipes) and
[conduit](https://hackage.haskell.org/package/conduit) streaming
abstractions have a problem: despite having special-purpose bracketing
operations they don't finalize promptly in the presence of exceptions.
[Bluefin](https://hackage.haskell.org/package/bluefin), by contrast,
has two benefits: it can bracket its streams with _general-purpose_
operations, and those brackets finalize promptly.

What does "finalize promptly" mean?  And why is it a problem to not
finalize promptly?  Read on to find out.

## Prompt finalization

When we have acquired, and have been using, a scarce resource (for
example a file handle) we need to "finalize it promptly", that is,
"release" or "clean it up" as soon as possible after we no longer need
it, so that it can be reused.  If a program component is guaranteed to
promptly finalize the resources it holds it can be called
"resource safe".

One element of the Haskell ecosystem that provides a form of prompt
finalization is
[`Control.Exception.bracket`](https://www.stackage.org/haddock/lts-22.18/base-4.18.2.0/Control-Exception.html#v:bracket).
The type and intended usage of `bracket` are:

```.hs
bracket ::
  -- | acquire the resource
  IO a  ->
  -- | release the resource
  (a -> IO b) ->
  -- | use the resource ("the body")
  (a -> IO c) ->
  -- | returns the result of the body
  IO c
```

When the `bracket` call executes the resource is firstly acquired and
then secondly used (the part of the program that uses the resource is
often called "the body") .  Thirdly, the resource is released once the
body finishes, regardless of whether the body finished normally or by
throwing an uncaught exception.  Below, `bracketExample` brackets the
usage of a file handle.  It opens a file, throws an exception if the
file is too big, and then subsequently closes the file.  (When it
opens and closes the file it also prints a message to tell us that it
is doing so.)

```.hs
bracketExample :: Integer -> IO ()
bracketExample n =
  bracket
    -- Acquire the file handle
    (do
        putStrLn "Opening file"
        openFile "/usr/share/dict/words" ReadMode
    )
    -- Release the file handle
    (\handle -> do
        putStrLn "Closing file"
        hClose handle)
    -- Use the file handle
    (\handle -> do
        size <- hFileSize handle
        when (size >= n) (error "Too big")
        putStrLn "Not too big")
```

If the body terminates normally, i.e. without an exception, then the
file is closed, as desired.

```
ghci> bracketExample (1000 * 1000)
Opening file
Not too big
Closing file
```

If the body terminates with an exception then the file is also closed,
as desired (and the exception is rethrown from `bracket`).

```
ghci> bracketExample 1000
Opening file
Closing file
*** Exception: Too big
```

## Bracketing when streaming

Bracketing of resources is a very useful, general concept.  Streaming
abstractions like those provided by pipes, conduit and Bluefin need to
support prompt finalization through some form of bracketing because
we want to be able to implement streaming computations that hold
resources.  For example, we might want to implement a producer that
yields all lines from a file.  To be resource safe, that producer must
close the handle to the file when it terminates, either normally or
through an exception.

`Control.Exception.bracket` works in `IO`, not in the monad
transformers provided by pipes and conduit, nor in the `Eff` monad
provided by Bluefin, so we can't use that bracketing function
directly.  Instead, the
[`pipes-safe`](https://hackage.haskell.org/package/pipes-safe) and
[`resourcet`](https://hackage.haskell.org/package/resourcet) libraries
provide the
[`SafeT`](https://hackage.haskell.org/package/pipes-safe-2.3.5/docs/Pipes-Safe.html#g:1)
and
[`ResourceT`](https://hackage.haskell.org/package/resourcet-1.3.0/docs/Control-Monad-Trans-Resource.html#t:ResourceT)
monad transformers that offer a form of prompt finalization that work
with the pipes and conduit ecosystems, and beyond.  Bluefin has its
own general-purpose bracketing operation, also called `bracket`.



Let's write a program so we can inspect the behaviour of pipes's,
conduit's and Bluefin's approaches to prompt finalization.  The
program will consist of a producer and a consumer.  The producer will
produce the numbers "1", "2", "3" and the consumer will print whatever
it receives.  Then we're going to connect the producer to the consumer
and run them, twice.  We expect to see the numbers "1", "2", "3"
printed to our terminal, twice.

Furthermore, the producer will be bracketed.  For simplicity we're not
going to bracket an actual resource, but we will print a message when
the resource is (or rather would be) acquired and released.  So, in
addition to "1", "2", "3", twice, we expect to see messages telling us
when the "resource" was acquired and released (twice each).

### Pipes implementation

Pipes is designed to work with `SafeT`, and provides a special-purpose
`bracket` operation for bracketing.  The implementation is as follows.


```.hs
pipes1 :: IO ()
pipes1 = runSafeT $ do
  runEffect $ do
    produce >-> consume
    produce >-> consume
  where
    produce :: Producer Int (SafeT IO) ()
    produce = do
      -- Special-purpose pipes bracket
      P.bracket
        (liftIO (putStrLn "Acquiring resource"))
        (\_ -> liftIO (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 .. 3] $ \i -> do
            P.yield i
        )

    consume :: (MonadIO m) => Consumer Int m ()
    consume = forever $ do
      r <- P.await
      liftIO (print r)
```

When we run `pipes1` everything works as expected.  The resource is
released (twice) as soon as it is no longer required, that is, as soon
as the producer's final element has been printed.


```
Acquiring resource
1
2
3
Releasing resource
Acquiring resource
1
2
3
Releasing resource
```

### Conduit implementation

The conduit implementation is similar.  Conduit is designed to work
with `ResourceT`, and provides a special-purpose `bracketP` operation
for bracketing.  The implementation is as follows.

```.hs
conduit1 :: IO ()
conduit1 = runResourceT $ do
  runConduit $ do
    produce .| consume
    produce .| consume
  where
    produce :: C.ConduitT i Int (ResourceT IO) ()
    produce = do
      -- Special-purpose conduit bracket
      C.bracketP
        (liftIO (putStrLn "Acquiring resource"))
        (\_ -> liftIO (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 .. 3] $ \i -> do
            C.yield i
        )

    consume :: (MonadIO m) => C.ConduitT Int o m ()
    consume = do
      C.await >>= \case
        Nothing -> pure ()
        Just r -> do
          liftIO (print r)
          consume
```

The output is the same as the pipes version so the bracketing works as
intended.

```
ghci> conduit1
Acquiring resource
1
2
3
Releasing resource
Acquiring resource
1
2
3
Releasing resource
```

### Bluefin implementation

The Bluefin implementation is also similar. Bluefin doesn't need
`SafeT` or `ResourceT`; its general-purpose `bracket` operation works
equally well for bracketing streaming operations as it does for any
other Bluefin operations.


```.hs
bluefin1 :: IO ()
bluefin1 = runEff $ \io -> do
  connectCoroutines (consume io) (produce io)
  connectCoroutines (consume io) (produce io)
  where
    produce ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      () ->
      Stream Int e2 ->
      Eff es ()
    produce io () y = do
     -- General-purpose Bluefin bracket
      B.bracket
        (effIO io (putStrLn "Acquiring resource"))
        (\_ -> effIO io (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 :: Int .. 3] $ \i -> do
            B.yield y i
        )

    consume ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      Coroutine () Int e2 ->
      Eff es ()
    consume io c = forever $ do
      r <- yieldCoroutine c ()
      effIO io (print r)
```

Like for pipes and conduit, finalization runs promptly.  The resource
is released each time the third element is printed.


```
ghci> bluefin1
Acquiring resource
1
2
3
Releasing resource
Acquiring resource
1
2
3
Releasing resource
```


## Exception unsafety

So far so good, but now the story gets worse for pipes and conduit.
One of the main roles of a bracketing operation is to provide prompt
finalization in the presence of exceptions.  Can pipes, conduit and
Bluefin's bracketing operations handle exceptions in streaming
operations?

To determine the answer we are going to adjust our programs so that
the producer throws an exception before it emits "3" and we're going
to catch the exception outside the producer, so it doesn't propagate
further.

Pipes and conduit do not deal well with this case.  Bluefin does.

### Pipes implementation

The pipes implementation is the same as before, except we throw an
exception in the producer when `i >= 3`, and catch it, at the
top-level of the producer definition, using pipe's special-purpose
`tryP` operation.

```.hs
data MyEx = MyEx deriving (Show)

instance E.Exception MyEx

pipes2 :: IO ()
pipes2 = runSafeT $ do
  runEffect $ do
    produce >-> consume
    produce >-> consume
  where
    produce :: Producer Int (SafeT IO) ()
    produce = do
      -- Special-purpose pipes try
      void $ P.tryP @_ @MyEx $ do
        -- Special-purpose pipes bracket
        P.bracket
          (liftIO (putStrLn "Acquiring resource"))
          (\_ -> liftIO (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 .. 3] $ \i -> do
              when (i >= 3) $
                liftIO (E.throwIO MyEx)
              P.yield i
          )

    consume :: (MonadIO m) => Consumer Int m ()
    consume = forever $ do
      r <- P.await
      liftIO (print r)
```

When we run `pipes2` we see a problem.  The resource should be
released after each time that "2" is printed, but first resource is
not released until the very end.  Oh dear.

```
ghci> pipes2
Acquiring resource
1
2
Acquiring resource
1
2
Releasing resource
Releasing resource
```


### Conduit implementation

We make the same adjustments to the conduit implementation as the
pipes implementation, using conduit's special-purpose `tryC`.


```.hs
conduit2 :: IO ()
conduit2 = runResourceT $ do
  runConduit $ do
    produce .| consume
    produce .| consume
  where
    produce :: C.ConduitT i Int (ResourceT IO) ()
    produce = do
      -- Special-purpose conduit try
      void $ C.tryC @_ @MyEx $ do
        -- Special-purpose conduit bracket
        C.bracketP
          (liftIO (putStrLn "Acquiring resource"))
          (\_ -> liftIO (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 .. 3] $ \i -> do
              when (i >= 3) (liftIO (E.throwIO MyEx))
              C.yield i
          )

    consume :: (MonadIO m) => C.ConduitT Int o m ()
    consume = do
      C.await >>= \case
        Nothing -> pure ()
        Just r -> do
          liftIO (print r)
          consume
```

The conduit example displays the same undesirable behaviour as the
pipes example.  The first resource should be released after "2" is
printed, but it is not released until the very end.


```
ghci> conduit2
Acquiring resource
1
2
Acquiring resource
1
2
Releasing resource
Releasing resource
```


### Bluefin implementation

How will Bluefin fare?  We make the same adjustments to the Bluefin
implementation as we made to the pipes and conduit implementations,
using Bluefin's general-purpose `try` to catch the exception thrown
when `i >= 3`.

```.hs
bluefin2 :: IO ()
bluefin2 = runEff $ \io -> do
  connectCoroutines (consume io) (produce io)
  connectCoroutines (consume io) (produce io)
  where
    produce ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      () ->
      Stream Int e2 ->
      Eff es ()
    produce io () y = do
      -- General-purpose Bluefin try
      void $ B.try $ \e -> do
        -- General-purpose Bluefin bracket
        B.bracket
          (effIO io (putStrLn "Acquiring resource"))
          (\_ -> effIO io (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 :: Int .. 3] $ \i -> do
              when (i >= 3) (B.throw e ())
              B.yield y i
          )

    consume ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      Coroutine () Int e2 ->
      Eff es ()
    consume io c = forever $ do
      r <- yieldCoroutine c ()
      effIO io (print r)
```

When we look at the Bluefin behaviour we see it behaves as desired!
The first resource is promptly finalized when it is no longer needed
(as it was in all three exception-free implementations).

```
ghci> bluefin2
Acquiring resource
1
2
Releasing resource
Acquiring resource
1
2
Releasing resource
```

## Bluefin and prompt finalization

### Bluefin's benefits

Bluefin has two benefits over pipes and conduits with regard to prompt
finalization of streaming resources.

1. Bluefin's `bracket` and `try` operations are general purpose: they
   work throughout the Bluefin ecosystem, not just for streaming.  The
   pipes/conduit `tryP`/`tryC` and `bracket`/`bracketP` operations are
   special purpose and work _only_ on pipes/conduits.

2. Bluefin streams finalize promptly, whereas the finalization of
   pipes and conduit might be called "promptish".  On exception they
   only release their resources at the end of the enclosing
   `runSafeT`/`runResourceT` block.

   Worse, this means the use of `ResourceT` and `SafeT` must leak out
   of the definition of the bracketed resource (in our case, the
   producer), breaking encapsulation.  Consequently, when using
   `runSafeT` and `runResourceT` you have to _remember_ to scope them
   as tightly as possible to the enclosed resources.  If you
   accidentally scope them too loosely you're holding on to resources
   for longer than necessary.  By contrast, the resource safety of the
   Bluefin bracketed resource is encapsulated. It's not possible to
   use it in a resource-unsafe manner.

### How?

How can Bluefin, which is a general-purpose effect system, provide
prompt finalization in streaming contexts when pipes and conduit,
which are special-purpose streaming libraries, cannot?

The ultimate reason is that Bluefin's [`Eff`
monad](https://hackage.haskell.org/package/bluefin-0.0.7.0/docs/Bluefin-Eff.html#t:Eff)
is `IO` under the hood.  Firstly, this means we can use the standard
`bracket` and `try` from `Control.Exception`, wrapped to work on `Eff`
instead of `IO`, to bracket and try all Bluefin operations.  No
special-purpose operations are needed.

Secondly, `connectCoroutines`, which connects a producer and a
consumer, works by using
[`Async.race`](https://hackage.haskell.org/package/async-2.2.5/docs/Control-Concurrent-Async.html#v:race)
to fork two threads which communicate in a synchronized fashion
through
[`MVar`](https://www.stackage.org/haddock/lts-22.34/base-4.18.2.1/Control-Concurrent-MVar.html#t:MVar)s.
If one thread is terminated by an uncaught exception then all brackets
active at the point the exception was thrown will already have had
their finalizers run.  Furthermore, `Async.race` ensures that the
other thread is terminated using an asynchronous exception, causing
the finalizers to run of all active brackets in that thread too.

### Conclusion

There is a common refrain in the Haskell community that "[lazy data
types are control
structures](https://old.reddit.com/r/haskell/comments/w39zwr/how_are_lists_sequences_from_containers_package/)".
This means, for example, that data structures like lazy lists, or
their effectful equivalents, pipes and conduit, can be seen as
"control structures" that determine the flow of control within an
executing program.  Given the difficulties that these sorts of
"control structures" have with prompt finalization it might worth
shifting away from that point of view to one in which the ultimate
control structures are those provided by `IO`-based effect systems
like Bluefin.

## References

* Michael Snoyman observed pipe's lack of prompt finalization in [The
 core flaws of pipes and
 conduit](https://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit>)
 (section "pipes: Prompt resource finalization"), but in the presence
 of exceptions, conduit has the same problem.

* Michael Snoyman: [pipes resource
  problems](https://www.yesodweb.com/blog/2013/10/pipes-resource-problems)

* Haskell Wiki: [Bracket
  pattern](https://wiki.haskell.org/Bracket_pattern)
