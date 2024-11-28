{-# LANGUAGE GHC2021 #-}

module HD1 where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Stream
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word

type Address = Int

-- Should be a list of length 8
type Data = [Word8]

data Constant = MkConstant Address Data

data Assembly a
  = MkAssembly (forall es. Stream Constant es -> Eff es a)

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
          case f a of MkAssembly f' -> f' s1
      )

constant :: Address -> Data -> Assembly ()
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant addr data_)

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

showAssemble :: Assembly () -> IO ()
showAssemble a = runEff $ \io -> do
  ap <- assemble a
  effIO io (print ap)

example :: Assembly ()
example = do
  -- Place bytes 0x00 to 0x07 at address 0x0000
  constant 0x0000 [0x00 .. 0x07]
  -- Place bytes 0x10 to 0x17 at address 0x0001
  constant 0x0001 [0x10 .. 0x17]

badExampleLength :: Assembly ()
badExampleLength = do
  -- Oh dear, this one is too short
  constant 0x0000 [0x00 .. 0x04]
  constant 0x0001 [0x10 .. 0x17]
  -- And this one is too short too
  constant 0x0002 [0x00 .. 0x04]

badExampleDuplication :: Assembly ()
badExampleDuplication = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]
  -- Oh dear, this is at the same address as another constant
  constant 0x0000 [0x10 .. 0x17]
