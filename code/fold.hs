{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Monad (forever, when)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable hiding (toList)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Traversable
import Data.Tuple
import Streaming.Prelude hiding (for, toList, show)
import qualified Streaming.Prelude
import qualified Streaming

foldl' :: (s -> a -> s) -> s -> [a] -> s
foldl' f s0 as =
  flip evalState s0 $ do
    for_ as $ \a -> do
      s <- get
      let s' = f s a
      put $! s'
    get

foldM :: Monad m => (s -> a -> m s) -> s -> [a] -> m s
foldM f s0 as =
  flip evalStateT s0 $ do
    for_ as $ \a -> do
      s <- get
      s' <- lift (f s a)
      put s'
    get

mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumL f s0 as =
  swap $ flip runState s0 $ do
    for as $ \a -> do
      s <- get
      let (s', b) = f s a
      put s'
      pure b

-- ghci> mapAccumRExample
-- ("start->4->3->2->1",["start->4->3->2->1","start->4->3->2","start->4->3","start->4"])
mapAccumRExample :: (String, [String])
mapAccumRExample =
  mapAccumR
    (\s i -> let s' = s ++ "->" ++ show i in (s', s'))
    "start"
    [1 .. 4]

mapAccumLM ::
  (Monad m) =>
  (s -> a -> m (s, b)) ->
  s ->
  [a] ->
  m (s, [b])
mapAccumLM f s0 as =
  fmap swap $ flip runStateT s0 $ do
    for as $ \a -> do
      s <- get
      (s', b) <- lift (f s a)
      put s'
      pure b

mapAccumLStreaming ::
  (Monad m) =>
  (s -> a -> m (s, b)) ->
  s ->
  Stream (Of a) m () ->
  Stream (Of b) m s
mapAccumLStreaming f s0 as =
  flip evalStateT s0 $ do
    flip Streaming.Prelude.mapM_ (Streaming.hoist (lift . lift) as) $ \a -> do
      s <- get
      (s', b) <- (lift . lift) (f s a)
      put s'
      lift (yield b)

    get

main :: IO ()
main = pure ()

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f as =
  toList $
    for_ as $ \a ->
      for_ (f a) $ \b ->
        yield b

concatMapM ::
  (Monad m) => (a -> Stream (Of b) m ()) -> [a] -> Stream (Of b) m ()
concatMapM f as =
  for_ as $ \a ->
    Streaming.Prelude.for (f a) $ \b ->
      yield b

-- Surely streaming has this?  It doesn't seem to have a _streaming_
-- version.
toList :: Stream (Of a) Identity r -> [a]
toList s = case runIdentity (Streaming.Prelude.uncons s) of
  Nothing -> []
  Just (h, rest) -> h : toList rest

iterate :: (s -> s) -> s -> [s]
iterate f s0 =
  toList $
    flip evalStateT s0 $
      forever $ do
        s <- get
        lift (yield s)
        put (f s)

-- N.B. flipped argument order
loopM :: (Monad m) => (s -> m (Either r s)) -> s -> m r
loopM f s0 =
  runEarlyReturnT $
    flip evalStateT s0 $
      forever $ do
        s <- get
        fs <- (lift . lift) (f s)
        s' <- lift (except fs)
        put s'

runEarlyReturnT :: (Monad m) => ExceptT r m r -> m r
runEarlyReturnT = fmap (either id id) . runExceptT

runEarlyReturn :: Either r r -> r
runEarlyReturn = either id id

loop :: (s -> Either r s) -> s -> r
loop f s0 =
  runEarlyReturn $
    flip evalStateT s0 $
      forever $ do
        s <- get
        s' <- lift (f s)
        put s'

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f as =
  toList $
    for_ as $ \a -> do
      for_ (f a) $ \b ->
        yield b

mapMaybeM ::
  (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as =
  fmap Streaming.Prelude.fst' $
    Streaming.Prelude.toList $
      for_ as $ \a -> do
        fa <- lift (f a)
        for_ fa $ \b ->
          yield b

any :: (a -> Bool) -> [a] -> Bool
any f as =
  runEarlyReturn $ do
    for_ as $ \a ->
      when (f a) $
        Left True

    pure False

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM f as =
  runEarlyReturnT $ do
    for_ as $ \a -> do
      fa <- lift (f a)
      when fa $
        throwE True

    pure False
