{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Needed nonly for defining an instance of MonadError
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Rank2Types #-}
-- ^^ Needed for superLocal

-- Tom: I implemented a concrete monad CoT and all its associated
-- transformer guff, before realising that 'localLocal' actually does
-- 'c5' on its own!  I guess this file is obsolete then.

-- Examples of various transformers, discussed in the paper

module TranEx where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Identity

-- ========================================================================
-- Example 2: Simple coroutines (threads)

data Step y m a = Done a | Yield y (CoT y m a)

data CoT y m a = CoT (m (Step y m a))

unCoT :: CoT y m a -> m (Step y m a)
unCoT (CoT m) = m

class Monad m => MonadCo y m | m -> y where
    yieldG :: y -> m ()
    toCo :: CoT y m a -> m a

yield :: Monad m => y -> CoT y m ()
yield = CoT . return . flip Yield (return ())

instance Monad m => Monad (CoT y m) where
    return  = CoT . return . Done
    CoT x >>= f = CoT $ do
      x' <- x
      case x' of
        Done a -> unCoT (f a)
        Yield y x' -> return (Yield y (x' >>= f))

instance Monad m => MonadCo y (CoT y m) where
    yieldG = yield
--    toCo = id
-- missing, but we don't need it!

instance MonadTrans (CoT y) where
    lift x = CoT ((return . Done) =<< x)

instance MonadCo y m => MonadCo y (ReaderT r m) where
    yieldG = lift . yieldG
    -- missing toCo

instance MonadReader r m => MonadReader r (CoT y m) where
  ask = lift ask
  -- missing local
  
toReader :: MonadReader r m => ReaderT r m a -> m a
toReader = localLocal id

-- Launch a thread and report its status
runC :: Monad m => CoT y m a -> m (Step y m a)
runC = unCoT

-- The implementation in terms of MTL
localLocal :: MonadReader a m => (a -> r) -> ReaderT r m b -> m b
localLocal f m = ask >>= runReaderT m . f

-- Just a simple 'hoist lift'
dupReader :: Monad m => ReaderT r m b -> ReaderT r (ReaderT a m) b
dupReader m = ReaderT (ReaderT . const . runReaderT m)

-- Like 'local' that only works at the top level.  Runs the reader in
-- a modified environment.
localLocal' :: Monad m => (a -> r) -> ReaderT r m b -> ReaderT a m b
localLocal' f m = localLocal f (dupReader m)

-- Even more dynamic example
c5 :: IO ()
c5 = runReaderT (loop =<< runC (th client)) (10::Int)
 where loop (Yield x k) = (liftIO . print) (show (x::Int)) >> local (+(1::Int)) (runC k) >>= loop
       loop (Done _)    = (liftIO . print) "Done"

       -- cl, client, ay are monomorphic bindings
       client :: (MonadCo r m, MonadReader r m) => m ()
       client = ay >> ay
       ay :: (MonadCo r m, MonadReader r m) => m ()
       ay     = ask >>= yieldG

       th :: (forall m. (MonadCo r m, MonadReader Int m) => m ()) -> ((MonadCo r m, MonadReader Int m) => m ())
       th cl = do
         cl
         v <- ask
         -- vv Rank2Types make it more awkward here
         if v > (20::Int) then cl else localLocal (+(5::Int)) cl
         if v > (20::Int) then return () else localLocal (+(10::Int)) (th cl)
