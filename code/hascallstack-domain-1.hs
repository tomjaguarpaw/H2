{-# LANGUAGE GHC2021 #-}

module HD1 where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Stream
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word

data CodeBlock
  deriving (Show)

type Address = Int

type Data = [Word8]

data Constant = MkConstant Address Data

data Assembly' a
  = MkAssembly (forall es. Stream CodeBlock es -> Stream Constant es -> Eff es a)

instance Functor Assembly' where
  fmap f (MkAssembly g) = MkAssembly (\s1 s2 -> fmap f (g s1 s2))

instance Applicative Assembly' where
  pure x = MkAssembly (\_ _ -> pure x)
  MkAssembly f <*> MkAssembly x =
    MkAssembly (\s1 s2 -> f s1 s2 <*> x s1 s2)

instance Monad Assembly' where
  return = pure
  MkAssembly m >>= f =
    MkAssembly
      ( \s1 s2 -> do
          a <- m s1 s2
          case f a of MkAssembly f' -> f' s1 s2
      )

type Assembly = Assembly' ()

constant :: Address -> Data -> Assembly
constant addr data_ = MkAssembly $ \_ c -> do
  yield c (MkConstant addr data_)

data AssembledProgram
  = MkAssembledProgram [CodeBlock] (Map Address Data)
  deriving (Show)

assemble ::
  Assembly ->
  Eff es AssembledProgram
assemble (MkAssembly k) = do
  (cbs, (cts, ())) <- yieldToList $ \cb -> do
    yieldToList $ \ct -> do
      k (mapHandle cb) (mapHandle ct)

  let m =
        Map.fromList (map (\(MkConstant addr data_) -> (addr, data_)) cts)

  pure (MkAssembledProgram cbs m)

showAssemble :: Assembly -> IO ()
showAssemble a = runEff $ \io -> do
  ap <- assemble a
  effIO io (print ap)

example = do
  constant 0x0000 [0x00 .. 0x07]
  constant 0x0001 [0x10 .. 0x17]
