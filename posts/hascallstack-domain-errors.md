# Domain errors with `HasCallStack`

At [Groq](https://groq.com/) one of the things we use Haskell for is
to provide an embedded domain specific assembly language for our novel
chip, the "LPU".

Amongst other things, the assembly language provides the ability to
place data (called "constants") at memory addresses on the chip.


Let's have a look at how we model this in Haskell.  For the purposes
of this article, let's say that a memory address (`Address`) as an
`Int`, and a constant value placed at an address (`Data`) is a list of
`Word8` of length 8 (i.e. a list of 8 bytes).  For a real-world
program we'd probably use newtypes or data definitions here, but to
keep things simple in this article we'll just use type synonyms.

```.hs
type Address = Int

-- Should be length 8.
-- We'll check that elsewhere.
type Data = [Word8]
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
monad.  Here I'm using Bluefin for the implementation.  This is
roughly equivalent to a `Writer [Constant]` monad.  In a real
assembler we'd be able to define other elements than just `Constant`s,
but let's keep it simple for now.

```.hs
data Assembly a
  = MkAssembly (forall es. Stream Constant es -> Eff es a)
```

For the monad instance, see the appendix below.

Then what should the function `constant` do?  It should yield the
arguments we give it into the stream, so we can later run the stream
and pick them up.

```.hs
constant :: Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant addr data_)
```

Let's say the assembly stage collects the constants together and puts
them in a `Map` keyed by `Address`:

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

Then we can write a function to run `assemble` and print the output.

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

That did what we hoped for, but the situation is less good on
badly-behaved examples.  For example, if the number of bytes we
provide is incorrect, then the error just passes silently.

```.hs
badExampleLength :: Assembly ()
badExampleLength = do
  -- Oh dear, this one is too short.
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
silently dropped.

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
to `constant` with the erroneous argument.

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

That works!  But it has two problems.  Firsty, we can only catch the
`ErrorCall` thrown by `error` where we have access to `IO`.  Ideally
we'd like to catch it in `showAssemble`, but we don't want that
function to use `IO`.  Secondly, there were two constants with
incorrect sizes, and when we threw the exception from `constant` it
stopped processing then and there, so we didn't get to pick up the
other one.  What can we do?

## Annotate the AST with the `CallStack`

Instead we can annotate our "AST" with the `CallStack`, so we rewrite
`constant` to

```.hs
constant :: (HasCallStack) => Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant callStack addr data_)
```

Then we can check for mis-sized constants in `Assemble` and throw a
Bluefin exception, which can be caught without depending on `IO`.

```.hs
assemble ::
  (e :> es) =>
  Assembly () ->
  Exception String e ->
  Eff es AssembledProgram
assemble (MkAssembly k) ex = do
  (constants, (errors, ())) <- yieldToList $ \yconstant ->
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

  -- If there were any errors, throw them
  case errors of
    [] -> pure ()
    _ -> throw ex (unlines errors)

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

## Duplicates

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
  let m = Map.fromListWith (<>) constants

  -- For each address with a constant, check if there is
  -- only one, or more than one, constant specified for
  -- that address.
  m' <- flip Map.traverseWithKey m $ \addr csData -> do
    case csData of
      -- Only one constant at this address
      (_, data_) Data.List.NonEmpty.:| [] -> pure data_
      -- Multiple constants at this address, so report the
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
defined.

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
