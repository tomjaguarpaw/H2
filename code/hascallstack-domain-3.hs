{-# LANGUAGE GHC2021 #-}

module HD3 where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.Stream
import Control.Monad (when)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import GHC.Stack

data CodeBlock
  deriving (Show)

type Address = Int

type Data = [Word8]

data Constant = MkConstant CallStack Address Data

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
  yield c (MkConstant callStack addr data_)

data AssembledProgram
  = MkAssembledProgram (Map Address Data)
  deriving (Show)

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

example :: Assembly ()
example = do
  constant 0x0000 [0x00 .. 0xff]
  constant 0x0001 [0x10 .. 0x17]

badExampleLength :: Assembly ()
badExampleLength = do
  -- Oh dear, this one is too long
  constant 0x0000 [0x00 .. 0x04]
  constant 0x0001 [0x10 .. 0x17]
  -- And this one is too long too
  constant 0x0002 [0x00 .. 0x04]

badExampleDuplication :: Assembly ()
badExampleDuplication = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]
  -- Oh dear, this is it the same address as another constant
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
