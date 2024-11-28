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

type Data = [Word8]

data Constant = MkConstant Address Data

data Assembly' a
  = MkAssembly (forall es. Stream Constant es -> Eff es a)

instance Functor Assembly' where
  fmap f (MkAssembly g) = MkAssembly (\s1 -> fmap f (g s1))

instance Applicative Assembly' where
  pure x = MkAssembly (\_ -> pure x)
  MkAssembly f <*> MkAssembly x =
    MkAssembly (\s1 -> f s1 <*> x s1)

instance Monad Assembly' where
  return = pure
  MkAssembly m >>= f =
    MkAssembly
      ( \s1 -> do
          a <- m s1
          case f a of MkAssembly f' -> f' s1
      )

type Assembly = Assembly' ()

constant :: Address -> Data -> Assembly
constant addr data_ = MkAssembly $ \c -> do
  yield c (MkConstant addr data_)

data AssembledProgram = MkAssembledProgram (Map Address Data)
  deriving (Show)

assemble ::
  Assembly ->
  Eff es AssembledProgram
assemble (MkAssembly k) = do
  (cts, ()) <- yieldToList $ \ct -> do
    k (mapHandle ct)

  let m =
        Map.fromList
          ( map
              ( \(MkConstant addr data_) ->
                  (addr, data_)
              )
              cts
          )

  pure (MkAssembledProgram m)

showAssemble :: Assembly -> IO ()
showAssemble a = runEff $ \io -> do
  ap <- assemble a
  effIO io (print ap)

example = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]

badExampleLength :: Assembly
badExampleLength = do
  constant 0x0000 [0x00 .. 0xff]
  constant 0x0001 [0x10 .. 0x17]
  constant 0x0002 [0x00 .. 0x0f]

badExampleDuplication = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0000 [0x10 .. 0x17]
