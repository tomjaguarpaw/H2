# Bluefin versus OOP

-- Tom Ellis, February 2025

In "[OOP is not that bad,
actually](https://osa1.net/posts/2024-10-09-oop-good.html)", Ömer
Sinan Ağacan describes a task that he says "mainstream
statically-typed OOP languages do well".  He defines OOP [object
oriented programming] as statically-typed programming with classes,
inheritance, subtyping and virtual calls.

Ultimately I interpret the article not as advocating for OOP, but
rather as advocating for programming against well-defined interfaces
that can be instantiated with a variety of implementations.  I'm
strongly in support.  However, I think the task is better solved by
Haskell, a statically-typed functional language, than by an OOP
language (as Ömer defines it).  In particular, I don't see inheritance
and subtyping as particularly valuable for this task.

Let's look at Ömer's example in more detail, in a Haskell context.

## A logger

We start with a basic logger interface.  In Haskell an interface is a
type, here `Logger`.  This is like the `Logger` type that Ömer
defined, except using the
[`Eff`](https://hackage.haskell.org/package/bluefin-0.0.9.0/docs/Bluefin-Eff.html#t:Eff)
type from my effect system Bluefin instead of `IO`.  (Most of what I
say in this article will apply equally well to `IO` or `Eff` and I'll
explain at the end why I think `Eff` is better.)

```.hs
type Severity = Int

newtype Logger e =
  -- Log a message with a severity
  MkLogger {logImpl :: String -> Severity -> Eff e ()}
```

Then we need a bit of ceremony to define a `Handle` instance and the
`log` function that we define in terms of the `Handle` implementation.
(This is in principle derivable using Template Haskell or Generics but
I haven't implemented that in Bluefin yet.  Mea culpa.  I'm including
this boilerplate in the article to be honest and explicit.)

```.hs
instance Handle Logger where
  mapHandle logger =
    MkLogger
      { logImpl = (fmap . fmap) useImpl (logImpl logger)
      }

log :: (e :> es) => Logger e -> String -> Severity -> Eff es ()
log = operationFrom logImpl
```

Then immediately we can define a function, `exampleWithLogger`, which
uses the `Logger` interface.  It prints some messages to stdout (using
`putStrLn`) and logs some messages to the `Logger` (using `log`).

```.hs
exampleWithLogger ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Logger e2 ->
  Eff es ()
exampleWithLogger io logger = do
  effIO io (putStrLn "Started Logger example")
  log logger "Mild Logger message" 0
  log logger "Severe Logger message" 10
  effIO io (putStrLn "Ended Logger example")
```

Having such a function is not useful until we have a way to
instantiate the `Logger` interface.  Here's one: it prints log
messages to stdout.

```.hs
withStdoutLogger ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
withStdoutLogger io k =
  useImplIn
    k
    MkLogger
      { logImpl =
          \msg sev ->
            -- Print log message to stdout
            effIO io (putStrLn (mkMsg msg sev))
      }
  where
    mkMsg msg sev =
      "Logger message: " ++ show sev ++ ": " ++ msg
```

([`useImplIn`](https://hackage.haskell.org/package/bluefin-0.0.14.1/docs/Bluefin-Compound.html#v:useImplIn)
and [`effIO
io`](https://hackage.haskell.org/package/bluefin-0.0.14.1/docs/Bluefin-IO.html#v:effIO)
are Bluefin incantations.  The same code using `IO` instead of `Eff`
wouldn't have them.)  I can instantiate the `Logger` interface and use
it to run the example, like this:

```.hs
runExampleWithLogger :: IO ()
runExampleWithLogger = runEff $ \io -> do
  -- "Instantiate" the Logger interface
  -- with a stdout logger
  withStdoutLogger io $ \logger -> do
    -- Use the Logger "instance"
    exampleWithLogger io logger
```

and the result is as expected:

```
Started Logger example
Log msg: 0: Mild Logger message
Log msg: 10: Severe Logger message
Ended Logger example
```

## A minimum severity logger

Another example that Ömer uses is a logger which only logs above a
certain severity.  Bluefin can do that too!  Ömer suggested that the
minimum severity logger should be created afresh, but I actually think
it's better to take an *existing* logger and wrap it into a minimum
severity logger.  That sounds more useful, more object-oriented, and
in any case more interesting, so let's do it:
`withLogAboveSeverityLogger` creates a logger that logs to an existing
`Logger` (passed in as an argument), but only when the severity is
above some minimum severity.

```.hs
withLogAboveSeverityLogger ::
  (e1 :> es) =>
  Severity ->
  Logger e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
withLogAboveSeverityLogger minSev logger k = do
  useImplIn
    k
    MkLogger
      { logImpl = \msg sev -> do
          when (sev >= minSev) $ do
            log logger msg sev
      }
```

Then we can make a stdout logger and restrict it so it only logs
messages of severity 5 and above:

```.hs
runExampleWithLogAboveSeverityLogger :: IO ()
runExampleWithLogAboveSeverityLogger = runEff $ \io -> do
  -- Make the stdout logger
  withStdoutLogger io $ \logger -> do
    -- Only log messages of severity 5 and above
    withLogAboveSeverityLogger 5 logger $ \severeLogger -> do
      -- Run the example with the restricted logger
      exampleWithLogger io severeLogger
```

The output is the same except the mild (severity 0) log message is
suppressed.

```
Started Logger example
Log msg: 10: Severe Logger message
Ended Logger example
```

## A file logger

Finally, Ömer defines a file logger.  Following the recipe seen above,
we first define an interface for this type of logger.  It contains a
`Logger` whose log operation will write to a file, and an effectful
operation which flushes writes to the open file.

```.hs
data FileLogger e = MkFileLogger
  { fileLoggerLogger :: Logger e,
    flushImpl :: Eff e ()
  }
```

This is "[composition, not
inheritance](https://en.wikipedia.org/wiki/Composition_over_inheritance)",
a famous design principle of OOP!  Again we have some ceremony.
Sorry.

```.hs
instance Handle FileLogger where
  mapHandle fileLogger =
    MkFileLogger
      { fileLoggerLogger =
          mapHandle (fileLoggerLogger fileLogger),
        flushImpl =
          useImpl (flushImpl fileLogger)
      }

flush :: (e :> es) => FileLogger e -> Eff es ()
flush = operationFrom flushImpl
```

To create a `FileLogger` we take a file name, get access to a
writeable file handle using a Bluefin `withFile` block, and use the
file handle to define the `Logger` and the flush operation.  Bluefin's
`withFile` is bracketed, so the file is closed automatically when
leaving `withFileLogger` (even if an exception is thrown).

```.hs
withFileLogger ::
  (e1 :> es) =>
  FilePath ->
  IOE e1 ->
  (forall e. FileLogger e -> Eff (e :& es) r) ->
  Eff es r
withFileLogger fp io k =
  -- Open a file for writing
  withFile io fp WriteMode $ \handle -> do
    -- Create the FileLogger
    useImplIn
      k
      MkFileLogger
        { fileLoggerLogger =
            MkLogger
              { logImpl =
                  \msg sev ->
                    -- Log to the open file
                    hPutStrLn handle (mkMsg msg sev)
              },
          flushImpl = do
            -- Diagnostic message for the sake of
            -- the example
            effIO io (putStrLn "Flushing FileLogger")
            -- Flush writes to the file
            hFlush handle
        }
  where
    mkMsg msg sev =
      "FileLogger message: " ++ show sev ++ ": " ++ msg
```

We want to be able to use functions that accept `Logger` with our
`FileLogger`.  But how can we?  Haskell doesn't have subtyping!
That's OK: Haskell has functions.  We just apply the function
`fileLoggerLogger`.  This achieves the same end as subtyping would,
but with an explicit use of function application rather than an
implicit use of the type system.

Then we can use a `FileLogger` with our function `exampleWithLogger`,
which expected a `Logger`

```.hs
exampleWithFileLogger ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  FileLogger e2 ->
  Eff es ()
exampleWithFileLogger io fileLogger = do
  -- Create a Logger from the FileLogger
  let logger = fileLoggerLogger fileLogger

  effIO io (putStrLn "Started FileLogger example")
  -- Log to the FileLogger
  log logger "Mild FileLogger message" 0

  withLogAboveSeverityLogger 5 logger $ \severeLogger -> do
    exampleWithLogger io severeLogger

  -- Flush the FileLogger
  flush fileLogger

  log logger "Severe FileLogger message" 10
  effIO io (putStrLn "Ended Logger example")
```

and run it (using the "file" `/dev/stdout`, so that all messages
appear directly on the console for the benefit of exposition).

```.hs
runExampleWithFileLogger :: IO ()
runExampleWithFileLogger = runEff $ \io -> do
  -- Create the FileLogger
  withFileLogger "/dev/stdout" io $ \fileLogger ->
    -- Use the FileLogger
    exampleWithFileLogger io fileLogger
```

We can see that the `FileLogger` is used for all messages, it is
flushed at the expected point, and the mild (severity 0) message
arising from `exampleWithLogger` is suppressed (as it was above in
`runExampleWithLogAboveSeverityLogger`).

```
Opening file
Started FileLogger example
File logger message: 0: Mild FileLogger message
Started Logger example
File logger message: 10: Severe Logger message
Ended Logger example
Flushing FileLogger
File logger message: 10: Severe FileLogger message
Ended Logger example
Closing file
```

## So, Bluefin versus OOP?

Before getting to the stage that we have now reached, Ömer suggested
that our approach is unworkable:

> unlike our OOP example, existing code that uses the `Logger` type
> and `log` function cannot work with this new [`FileLogger`]
> type. There needs to be some refactoring, and how the user code will
> need to be refactored depends on how we want to expose this new type
> to the users.

I don't understand why.  The approach taken in this article seems
perfectly workable to me, even very natural, and doesn't require any
refactoring of existing code.  It uses explicit function application
instead of implicit subtyping.

The one element that one might think OOP languages do better is
avoiding the explicit function application.  But we don't need
subtyping, even for that!  All we need is a way for a value to
implicitly conform to some interface, in Haskell a type class.  For
example:

```.hs
class IsLogger h where
  isLogger :: h e -> Logger e

instance IsLogger FileLogger where
  isLogger = fileLoggerLogger
```

And we could define functions to take an instance of `IsLogger` rather
than a concrete `Logger`, for example change

```.hs
exampleWithLogger ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Logger e2 ->
  Eff es ()
```

to

```.hs
exampleWithLogger ::
  (e1 :> es, e2 :> es, IsLogger logger) =>
  IOE e1 ->
  logger e2 ->
  Eff es ()
```

*That* would involve some refactoring, if we've already defined a
suite of functions that accept `Logger`, but it's not required, even
if potentially convenient.  I'm inclined to say that being explicit is
clearer anyway, but then I like neither the implicit subtyping
approach of OO style, nor the implicit type class approach of many
Haskell libraries, to begin with.

## Bluefin or Haskell?

Even if we like this style of "programming against well-defined
interfaces that can be instantiated with a variety of
implementations", are we really getting the benefits from Bluefin, or
just Haskell?  Well, a significant part comes from Haskell, but there
are a couple of notable additional benefits of Bluefin: effect
tracking and resource safety.

### Effect tracking

Firstly, Bluefin enforces strict controls, through the type system,
over what functions can and cannot do.  For example, we can add a
logger that yields log messages to a Bluefin `Stream`:

```.hs
streamLogger ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
streamLogger stream k =
  useImplIn
    k
    MkLogger
      { logImpl = \msg sev ->
          yield stream (mkMsg msg sev)
      }
  where
    mkMsg msg sev =
      "streamLogger message: " ++ show sev ++ ": " ++ msg
```

Later we can choose what to do with the elements from the stream.
Here we print them.

```.hs
runExampleStreamLogger :: IO ()
runExampleStreamLogger = runEff $ \io -> do
  forEach
    ( \stream -> streamLogger stream $ \logger -> do
        exampleWithLogger io logger
    )
    ( \logMsg -> do
        effIO io (putStrLn logMsg)
    )
```

The end product is the same as `runExampleWithLogger` above.

```
Started Logger example
streamLogger message: 0: Mild Logger message
streamLogger message: 10: Severe Logger message
Ended Logger example
```

But the way we have factored the program is different.  From the
program structure it is clear that using `streamLogger` does not
perform any `IO` (there is no `IOE` argument to `streamLogger`) even
though other parts of the program do perform `IO`.  This fine-grained
tracking of effects is not possible if you use the raw `IO` monad
(anything that runs in `IO` can perform any possible effect) nor is
such tracking possible in any "mainstream, statically-typed OOP
language".

### Resource safety

Secondly, the file handle supplied by
[`withFile`](https://hackage.haskell.org/package/bluefin-0.0.14.1/docs/Bluefin-System-IO.html#v:withFile)
(used in the definition of `withFileLogger`) is guaranteed not to
escape its scope, that is, it's not possible to use it after
`withFileLogger` has returned.  That guarantee is not provided by
Haskell's standard
[`System.IO.withFile`](https://www.stackage.org/haddock/lts-22.39/base-4.18.2.1/System-IO.html#v:withFile).

## Worse?

In what ways is Bluefin worse than an OOP language, or just pure Haskell?

Well, we've seen that there is ceremony and boilerplate involved in
defining and using Bluefin effects.  Much of this can in principle be
addressed by generating the boilerplate using Template Haskell or
Generics, but tracking fine-grained effects at the type system is
always going to be less convenient than using plain `IO` (in Haskell),
or than using nothing (in every other language).

Setting up the "subtyping" relationship using `IsLogger` also requires
some boilerplate.  In OO languages it comes for free, syntactically
free anyway.

I think these costs are worth paying, but I won't be surprised if
others have different preferences.

## Conclusion

For my purposes, I'm happy to say that "[Bluefin is better than OOP,
actually](https://osa1.net/posts/2024-10-09-oop-good.html)".

## Appendix

The code in this article uses the following convenience function,
which may or may not be added to Bluefin at a later date.

```.hs
operationFrom ::
  (Handle h, e :> es) =>
  (h es -> t) ->
  h e ->
  t
operationFrom f = f . mapHandle
```

## Acknowledgements

* Ömer, for writing the original article

* Enes Bayram, for [suggesting the approach of this article on Haskell
  Reddit](https://old.reddit.com/r/haskell/comments/1fzy3fa/oop_is_not_that_bad_actually/lr55hko/)
