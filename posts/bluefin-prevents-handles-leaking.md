# Bluefin prevents handles leaking

Haskell's `System.IO.withFile` has a nice resource safety property.
It also has a problem which its Bluefin equivalent fixes.  Let's have
a look.

## `withFile`

[`System.IO.withFile`](https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html#v:withFile)
has the following type

```.hs
withFile ::
  FilePath ->
  IOMode ->
  -- | The "body"
  (Handle -> IO r) ->
  IO r
```

It allows us to open a file by its path, and then interact with it in
the "body" of the `withFile` using its "file handle" (of type
[`System.IO.Handle`](https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html#t:Handle)). When
the body has finished `withFile` will automatically close the file,
releasing its `Handle`.  File handles are a scarce resource so it's
important to release them promptly.  `withFile` saves us work by
taking care of that for us automatically.  Here's an example

```.hs
useHandle :: IO ()
useHandle = do
  System.IO.withFile "/tmp/message" WriteMode $ \handle -> do
    -- This do block is called the "body" of the withFile
    System.IO.hPutStrLn handle "Hello!"

  -- handle has been closed by the time we get here
```

Even better, `withFile` is "bracketed".  That means that it releases
its `Handle` even when the body it terminated by an exception:

```.hs
useHandleException :: IO ()
useHandleException = do
  System.IO.withFile "/tmp/message" WriteMode $ \handle -> do
    -- This do block is called the "body" of the withFile
    System.IO.hPutStrLn handle "Hello!"
    error "Oh dear!"

  -- handle has been closed by the time we get here,
  -- even though the body was terminated by an exception
```

But all is not good.  Nothing stops us leaking the handle from
`withFile` and attempting to access it after it has been closed.  For
example, the following code errors out at run time:

```.hs
leakHandle :: IO ()
leakHandle = do
  handle <-
    System.IO.withFile "/tmp/output-file" WriteMode $ \handle -> do
      pure handle

  System.IO.hPutStrLn handle "Still open?"
```

```
ghci> leakHandle
*** Exception: /tmp/output-file: hPutStr: illegal operation (handle is closed)
```

## Bluefin to the rescue

[Bluefin's version of
`withFile`](https://hackage.haskell.org/package/bluefin-0.0.10.0/docs/Bluefin-System-IO.html#v:withFile)
doesn't suffer from the same problem.  If we try to write code that
leaks the handle from `withFile` we get a type error.

```.hs
leakHandleBf :: IO ()
leakHandleBf = runEff $ \io -> do
  handle <-
    Bluefin.System.IO.withFile
      io
      "/tmp/output-file"
      WriteMode
      $ \handle -> do
        pure handle

  Bluefin.System.IO.hPutStrLn handle "Still open?"
```

```
/home/tom/H2/code/logger.hs:208:10: error:
   • Couldn't match type ‘e0’ with ‘e1’
     Expected: Handle e0
       Actual: Handle e1
   • because type variable ‘e1’ would escape its scope
```

How does that happen?  It's because of the type of Bluefin's
`withFile`, which is:

```.hs
withFile ::
  (e1 :> es) =>
  IOE e1 ->
  FilePath ->
  IOMode ->
  (forall e. Handle e -> Eff (e :& es) r) ->
  Eff es r
```

The type of the handle that Bluefin's `withFile` provides to its body
is `Handle e` (that's a
[`Bluefin.System.IO.Handle`](https://hackage.haskell.org/package/bluefin-0.0.10.0/docs/Bluefin-System-IO.html#t:Handle)),
i.e. it is tagged with the Bluefin effect type `e`. The `forall` that
binds `e` prevents us from smuggling the handle out as part of `r`,
the return type of the block.  This technique is an augmented version
of the one used by
[`runST`](https://www.stackage.org/haddock/lts-22.39/base-4.18.2.1/Control-Monad-ST.html#v:runST)
to avoid leaking `STRef`s.

So `System.IO.withFile` has a neat resource safety property, and
Bluefin's equivalent is even safer!

## See also

* [Bluefin streams finalize
  promptly](../bluefin-streams-finalize-promptly/)
