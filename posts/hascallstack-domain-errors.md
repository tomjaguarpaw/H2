# Domain errors with `HasCallStack`

## Introduction

[`HasCallStack`](https://www.stackage.org/haddock/lts-22.43/base-4.18.2.1/GHC-Stack.html#t:HasCallStack)
is a feature in Haskell's compiler, GHC, that is frequently used to
annotate exceptions with the call stack at the point in the program
from which they were thrown.  For example, I can define an exception
type that, as well as a message, contains a `CallStack`:

```.hs
data MyException = MkMyException String CallStack
  deriving Show

instance Exception MyException
```

And define a way of creating it that captures the current call stack:

```.hs
mkMyException :: HasCallStack => String -> MyException
mkMyException msg = MkMyException msg callStack
```

Then when I catch the exception I can inspect the call stack:

```.hs
myExceptionExample :: IO ()
myExceptionExample = do
  Control.Exception.catch @MyException
    (throwIO (mkMyException "Hello"))
    (\(MkMyException msg cs) -> do
        putStrLn msg
        putStrLn "Thrown from:"
        putStrLn (prettyCallStack cs))
```

```
ghci> myExceptionExample
Hello
Thrown from:
CallStack (from HasCallStack):
  mkMyException, called at code/hascallstack-domain-2.hs:28:15 in fake-package-0-inplace:HD2
```

The feature makes debugging easier by allowing the origin of of an
exception to be discovered.  But it is useful for more than just
exceptions: it is also useful for error messages in domain specific
languages.  In this article we'll look at an example of such a use
case.

## An assembly EDSL

At [Groq](https://groq.com/), one of the things we use Haskell for is
to provide an embedded domain specific assembly language (an "assembly
EDSL") for our novel chip, the "LPU".

The assembly language allows us to define a program, which consists of
a sequence of instructions to run on each processor on the chip and a
collection of data values (called "constants") that are loaded
alongside the program to particular addresses in the chip's memory.

Let's have a look at how we can implement part of this in Haskell,
restricting ourselves, for simplicity, to specifying the constant
values and addresses.  For the purposes of this article, we'll say
that a memory address (`Address`) is an `Int`, and a constant value
placed at an address (`Data`) is a list of `Word8` of length 8 (i.e. a
list of 8 bytes).  In practice we would probably use newtype or data
definitions but to keep things simple in this article we'll just use
type synonyms.

```.hs
type Address = Int

-- Should be length 8.
-- We'll check that elsewhere.
type Data = [Word8]

-- A "constant" is some data residing at
-- a particular address
data Constant = MkConstant Address Data
```

We want to be able to define constants using a convenient syntax, for
example like this:

```.hs
example :: Assembly ()
example = do
  -- Place bytes 0x00 to 0x07 at address 0x0000
  constant 0x0000 [0x00 .. 0x07]
  -- Place bytes 0x10 to 0x17 at address 0x0001
  constant 0x0001 [0x10 .. 0x17]
```

Let's see how we can arrange that.  Firstly, we'll need an `Assembly`
monad.  Here I'm using Bluefin for the implementation.  The
implementation is roughly equivalent to a `Writer [Constant]` monad
(from the `transformers` library) or a `Stream (Of Constant)` monad
(from the `streaming` library). (For the monad instance, see the
[appendix](#appendix).)

```.hs
data Assembly a
  = MkAssembly (forall es. Stream Constant es -> Eff es a)
```

Then what should the function `constant` do?  It should yield the
arguments we give it into the stream, so that when we run the stream
we pick them up.

```.hs
constant :: Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant addr data_)
```

For the assembly stage, let's collect the constants together and put
them in a `Map` (from the `containers` library) keyed by `Address`:

```.hs
data AssembledProgram
  = MkAssembledProgram (Map Address Data)
  deriving (Show)

assemble ::
  Assembly () ->
  Eff es AssembledProgram
assemble (MkAssembly k) = do
  (constants, ()) <- yieldToList $ \ct ->
    forEach (useImpl . k) $ \(MkConstant addr data_) ->
      yield ct (addr, data_)

  let m = Map.fromList constants

  pure (MkAssembledProgram m)
```

And to see the output of `assemble` we write `showAssemble`:

```.hs
showAssemble :: Assembly () -> IO ()
showAssemble a = runEff $ \io -> do
  ap <- assemble a
  effIO io (print ap)
```

```
> showAssemble example
MkAssembledProgram (fromList [(0,[0,1,2,3,4,5,6,7]),(1,[16,17,18,19,20,21,22,23])])
```

That was what we hoped for! Our original `example` above had two
constants, on addresses `0x0000` and `0x0001`, exactly as the result
shows.  Unfortunately the situation is less good on badly-behaved
examples: for example, if the number of bytes we provide is incorrect,
then the error just passes silently.  We get invalid-length lists in
the output:

```.hs
badExampleLength :: Assembly ()
badExampleLength = do
  -- Oh dear, this list is too short.
  -- It's only length 4 but should be 8.
  constant 0x0000 [0x00 .. 0x04]
  constant 0x0001 [0x10 .. 0x17]
  -- And this one is too short too.
  constant 0x0002 [0x00 .. 0x04]
```

```
> showAssemble badExampleLength
MkAssembledProgram (fromList [(0,[0,1,2,3,4]),(1,[16,17,18,19,20,21,22,23]),(2,[0,1,2,3,4])])
```

Or if we specify two constants at the same address then one of them is
silently dropped:

```.hs
badExampleDuplication :: Assembly ()
badExampleDuplication = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]
  -- Oh dear, this is at the same
  -- address as another constant
  constant 0x0000 [0x10 .. 0x17]
```

```.hs
> showAssemble badExampleDuplication
MkAssembledProgram (fromList [(0,[16,17,18,19,20,21,22,23]),(1,[16,17,18,19,20,21,22,23])])
```

## Throwing an exception

Let's tackle the incorrect size issue first.  We can amend `constant`
to throw an exception when the list provided has the wrong length.
Using `HasCallStack` we can helpfully include the location of the call
to `constant` which had an erroneous argument.

```.hs
constant :: (HasCallStack) => Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  let l = length data_
  when (l /= 8) $ do
    let cs = callStack
    error $
      unlines $
        [ "Wrong size constant",
          "Expected: 8",
          "Actual: " <> show l,
          "In:"
        ]
          <> showCallStack cs

  yield c (MkConstant addr data_)
```

Then we can catch the exception thrown by `error` when we come to
print out the result of the assembly.

```.hs
showAssemble :: Assembly () -> IO ()
showAssemble a = runEff $ \io -> do
  handle
    (effIO io . putStrLn . (\(ErrorCall s) -> s))
    ( \ex -> do
        rethrowIO io ex $ do
          ap <- assemble a
          effIO io (print ap)
    )
```

```
> showAssemble badExampleLength
Wrong size constant
Expected: 8
Actual: 5
In:
constant at 92:3
```

That works!  We see the line and column number of one of the mis-sized
constants.  But there are still two problems.  Firstly, we can only
catch the `ErrorCall` thrown by `error` where we have access to `IO`.
Ideally we'd like to catch it in `showAssemble`, but we don't want
that function to use `IO`.  Secondly, there were two constants with
incorrect sizes, and when we ran `error` in `constant` it stopped
processing then and there, so we didn't get to pick up the other one.
What can we do?

## Annotate with the `CallStack`

Instead of throwing an exception annotated with the `CallStack` we can
annotate our constants with the `CallStack`. To do so we rewrite
`constant` to:

```.hs
constant :: (HasCallStack) => Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant callStack addr data_)
```

Then we can move the check for mis-sized constants into `assemble`.
If we find any we throw an exception which mentions the source
locations of the definitions of *all* of them, not just one of them.

```.hs
assemble ::
  (e :> es) =>
  Assembly () ->
  Exception String e ->
  Eff es AssembledProgram
assemble (MkAssembly k) ex = do
  (constants, (errorLines, ())) <- yieldToList $ \yconstant ->
    yieldToList $ \yerror ->
      forEach (useImpl . k) $ \(MkConstant cs addr data_) -> do
        let l = length data_

        yield yconstant (addr, data_)

        -- If the constant was the wrong length
        -- yield some error message lines
        when (l /= 8) $ do
          traverse_
            (yield yerror)
            ( [ "Wrong size constant",
                "Expected: 8",
                "Actual: " <> show l,
                "In:"
              ]
                <> showCallStack cs
                <> [""]
            )

  -- If there were any error lines, throw them
  case errorLines of
    [] -> pure ()
    _ -> throw ex (unlines errorLines)

  -- If not, gather the constants into a map
  let m = Map.fromList constants

  pure (MkAssembledProgram m)

showAssemble :: Assembly () -> IO ()
showAssemble a = runEff $ \io -> do
  handle (effIO io . putStr) $ \ex -> do
    ap <- assemble a ex
    effIO io (print ap)
```

```.hs
> showAssemble badExampleLength
Wrong size constant
Expected: 8
Actual: 5
In:
constant at 101:3

Wrong size constant
Expected: 8
Actual: 5
In:
constant at 104:3
```

That works!  We can see information about both of the mis-sized
constants.

## Detecting duplicates

We now have enough information in the body of `assemble` to present
the user with the source location of constants which share the same
address.  To arrange that, we keep the first part of `assemble` almost
the same as before; in the second part we gather all constants for
each address into a non-empty list, and throw an exception if any such
list was more than one element long.

```.hs
assemble ::
  (e :> es) =>
  Assembly () ->
  Exception String e ->
  Eff es AssembledProgram
assemble (MkAssembly k) ex = do
  -- This part very similar to before

  (constants, (errors, ())) <- yieldToList $ \yconstant ->
    yieldToList $ \yerror ->
      forEach (useImpl . k) $ \(MkConstant cs addr data_) -> do
        let l = length data_

        -- We put the call stack and data in a non-empty list
        -- before yielding it.
        yield yconstant (addr, pure (cs, data_))

        -- If the constant was the wrong length
        -- yield some error message lines
        when (l /= 8) $ do
          traverse_
            (yield yerror)
            ( [ "Wrong size constant",
                "Expected: 8",
                "Actual: " <> show l,
                "In:"
              ]
                <> showCallStack cs
                <> [""]
            )

  -- If there were any errors, throw them
  case errors of
    [] -> pure ()
    _ -> throw ex (unlines errors)

  -- This part is new, and detects duplicate constants

  -- We gather all the values for a given key (address)
  -- into a non-empty list.  If such a list has more than
  -- one element, that indicates an error.
  let m :: Map Address (NonEmpty (CallStack, Data))
      m = Map.fromListWith (<>) constants

  -- For each address with a constant, check if there is
  -- only one, or more than one, constant specified for
  -- that address.
  m' <- flip Map.traverseWithKey m $ \addr csData -> do
    case csData of
      -- Only one constant at this address. That's fine.
      (_, data_) Data.List.NonEmpty.:| [] -> pure data_
      -- Multiple constants at this address. Report the
      -- error
      _ -> do
        throw ex $
          unlines $
            [ "Duplicate constants at address " <> show addr
            ]
              <> concatMap
                (\(cs, _) -> showCallStack cs)
                (toList csData)

  pure (MkAssembledProgram m')
```

```
> showAssemble badExampleDuplication
Duplicate constants at address 0
constant at 123:3
constant at 121:3
```

That works very nicely!  We get to see the address with duplicated
constants, and the source locations where those constants were
defined.  We could improve our implementation by throwing an exception
containing *all* addresses with duplicated constants (currently,
processing stops at the first such address), but we won't bother to do
so in this article.

## Conclusion

`HasCallStack` is useful not only for error messages arising from
exceptions, it can be helpful for a wider class of error messages.  If
you capture a call stack alongside data you can display annotated
error messages when you detect error conditions during processing that
data.

## References

* [Debugging your Haskell application with
  debuggable](https://www.well-typed.com/blog/2024/12/debuggable/)
  shows many other uses to which you can put `HasCallStack`, including
  tracing messages.

* [User `dpwiz` explains the same
  idea](https://github.com/NorfairKing/haskell-dangerous-functions/issues/17#issuecomment-975568937)
  at GitHub

## Appendix

```.hs
instance Functor Assembly where
  fmap f (MkAssembly g) =
    MkAssembly (\s1 -> fmap f (g s1))

instance Applicative Assembly where
  pure x = MkAssembly (\_ -> pure x)
  MkAssembly f <*> MkAssembly x =
    MkAssembly (\s1 -> f s1 <*> x s1)

instance Monad Assembly where
  return = pure
  MkAssembly m >>= f =
    MkAssembly
      ( \s1 -> do
          a <- m s1
          case f a of
            MkAssembly f' -> f' s1
      )
```
