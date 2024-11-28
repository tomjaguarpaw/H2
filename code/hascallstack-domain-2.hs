{-# LANGUAGE GHC2021 #-}

module HD2 where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.Stream
import Control.Exception hiding (handle)
import Control.Monad (when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import GHC.Stack

data MyException = MkMyException String CallStack
  deriving Show

instance Control.Exception.Exception MyException

mkMyException :: HasCallStack => String -> MyException
mkMyException msg = MkMyException msg callStack

myExceptionExample :: IO ()
myExceptionExample = do
  Control.Exception.catch @MyException
    (throwIO (mkMyException "Hello"))
    (\(MkMyException msg cs) -> do
        putStrLn msg
        putStrLn "Thrown from:"
        putStrLn (prettyCallStack cs))

-- ghci> myExceptionExample
-- Hello
-- Thrown from:
-- CallStack (from HasCallStack):
--   mkMyException, called at code/hascallstack-domain-2.hs:28:15 in fake-package-0-inplace:HD2

type Address = Int

type Data = [Word8]

data Constant = MkConstant Address Data

data Assembly a
  = MkAssembly (forall es. Stream Constant es -> Eff es a)

instance Functor Assembly where
  fmap f (MkAssembly g) = MkAssembly (\s1 -> fmap f (g s1))

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
          case f a of MkAssembly f' -> f' s1
      )

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

data AssembledProgram
  = MkAssembledProgram (Map Address Data)
  deriving (Show)

assemble ::
  Assembly () ->
  Eff es AssembledProgram
assemble (MkAssembly k) = do
  (constants, ()) <- yieldToList $ \yconstant ->
    forEach (useImpl . k) $ \(MkConstant addr data_) ->
      yield yconstant (addr, data_)

  let m = Map.fromList constants

  pure (MkAssembledProgram m)

showAssemble :: Assembly () -> IO ()
showAssemble a = runEff $ \io -> do
  handle
    (effIO io . putStrLn . (\(ErrorCall s) -> s))
    ( \ex -> do
        rethrowIO io ex $ do
          ap <- assemble a
          effIO io (print ap)
    )

example :: Assembly ()
example = do
  constant 0x0000 [0x00 .. 0xff]
  constant 0x0001 [0x10 .. 0x17]

badExampleLength :: Assembly ()
badExampleLength = do
  constant 0x0000 [0x00 .. 0x04]
  constant 0x0001 [0x10 .. 0x17]
  constant 0x0002 [0x00 .. 0x04]

badExampleDuplication :: Assembly ()
badExampleDuplication = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]
  constant 0x0000 [0x10 .. 0x17]

showCallStack :: CallStack -> [String]
showCallStack cs =
  flip map (getCallStack cs) $ \(fn, srcloc) ->
    unwords
      [ fn,
        "at",
        show (srcLocStartLine srcloc)
          <> ":"
          <> show (srcLocStartCol srcloc)
      ]
