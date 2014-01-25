{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Needed nonly for defining an instance of MonadError
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Rank2Types #-}
-- ^^ Needed for superLocal

-- Examples of various transformers, discussed in the paper

module TranEx where

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Identity

-- ========================================================================
-- Example 1: ErrorT and non-determinism

newtype TooBig = TooBig Int deriving Show
instance Error TooBig

ex2_MTL :: MonadError TooBig m => Int -> m Int
ex2_MTL v = if v > 5 then throwError (TooBig v)
            else return v

localCatch :: Monad m => ErrorT e m a -> (e -> m a) -> m a
localCatch m h = runErrorT m >>= \x -> case x of
  Left  e -> h e
  Right a -> return a

exRec_MTL :: MonadError TooBig m => ErrorT TooBig m Int -> m Int
exRec_MTL m = m `localCatch` handler
  where handler (TooBig n) | n <= 7 = return n
        handler e = throwError e

class Monad m => MonadChoose m where
  choose_MTL :: [a] -> m a

instance Monad m => MonadChoose (ListT m) where
  choose_MTL xs = ListT (return xs)

instance (Error e, MonadChoose m) => MonadChoose (ErrorT e m) where
  choose_MTL = lift . choose_MTL

computation :: (MonadChoose m, MonadError TooBig m) => [Int] -> m Int
computation = (ex2_MTL =<<) . choose_MTL

ex4_1_MTL :: Either TooBig [Int]
ex4_1_MTL = runIdentity . runErrorT . runListT . computation $
            [5, 7, 1]

ex4_2_MTL :: Either TooBig [Int]
ex4_2_MTL = runIdentity . runErrorT . runListT . exRec_MTL . computation $
            [5, 7, 1]

ex2r_MTL :: Either TooBig [Int]
ex2r_MTL = runIdentity . runErrorT . runListT . exRec_MTL . computation $
           [5, 7, 11, 1]

-- ========================================================================
-- Example 2: Simple coroutines (threads)

-- Status of a thread: done or reports the value of the type a
-- (For simplicity, a co-routine only reports a value but accepts unit)
data Y m a = Done | Y a (() -> m (Y m a))

-- Trivial boilerplate, providing instances for the CoT newtype in
-- terms of ContT
newtype CoT y m a = CoT { unCoT :: ContT (Y m y) m a }

class Monad m => MonadCo y m | m -> y where
    yieldG :: y -> m ()

instance Monad m => Monad (CoT y m) where
    return = CoT . return
    m >>= f = CoT (unCoT m >>= unCoT . f)

instance Monad m => MonadCo y (CoT y m) where
    yieldG = yield

instance MonadTrans (CoT y) where
    lift = CoT . lift

instance MonadCo y m => MonadCo y (ReaderT r m) where
    yieldG = lift . yieldG

instance (Monad m, MonadReader r m) => MonadReader r (CoT y m) where
    ask = CoT (lift ask)
    local f = CoT . local f . unCoT
    reader = CoT . lift . reader

-- Oleg's ConT combinators
shift :: ((a -> m r) -> m r) -> ContT r m a
shift = ContT

-- Yield a value and suspend
yield :: Monad m => a -> CoT a m ()
yield x = CoT $ shift (\k -> return $ Y x k)

-- Launch a thread and report its status
runC :: Monad m => CoT a m b -> m (Y m a)
runC m = runContT (unCoT m) (\_ -> return Done)

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

th3_explicit :: Monad m => CoT Int (ReaderT Int m) ()
th3_explicit = ay1 >> ay1 >> localLocal (+10) (ay2 >> ay2)
 where ay1 :: Monad m => CoT Int (ReaderT Int m) ()
       ay1 = lift ask >>= yield
       ay2 :: Monad m => ReaderT Int (CoT Int m) ()
       ay2 = ask >>= lift . yield

-- vv NB very general type signature
th3_MTL :: (MonadCo Int m, MonadReader Int m) => m ()
--th3_MTL :: Monad m => CoT Int (ReaderT Int m) ()
th3_MTL = ay >> ay >> localLocal (+10) (ay >> ay)
 where ay :: (MonadCo r m, MonadReader r m) => m ()
       ay = ask >>= yieldG

th3_Bad :: Monad m => CoT Int (ReaderT Int m) ()
th3_Bad = local (+10) (ay >> ay >> ay)
  where ay = ask >>= yieldG

c3_MTL :: IO ()
c3_MTL = runReaderT (loop =<< runC th3_MTL) 10
 where loop (Y x k) = liftIO (print x) >> (k ()) >>= loop
       loop Done    = liftIO (print "Done")

c31_MTL :: IO ()
c31_MTL = runReaderT (loop =<< runC th3_MTL) 10
 where --loop :: (MonadReader Int m, MonadIO m) => Y m Int -> m ()
       loop :: Y (ReaderT Int IO) Int -> ReaderT Int IO ()
       loop (Y x k) = liftIO (print x) >> local (+1) (k ()) >>= loop
       loop Done    = liftIO (print "Done")

-- This shows the problem more clearly actually.  When the ReaderT is
-- embedded in an outer layer, 'local' only modifies up until the
-- first operation in the outer layer.  In c31_Bad, once the outer
-- layer has been removed with 'runC' local modifies everything as
-- intended.
c31_Bad :: IO ()
c31_Bad = runReaderT (loop =<< runC th3_Bad) 10
 where --loop :: (MonadReader Int m, MonadIO m) => Y m Int -> m ()
       loop :: Y (ReaderT Int IO) Int -> ReaderT Int IO ()
       loop (Y x k) = liftIO (print x) >> local (+1) (k ()) >>= loop
       loop Done    = liftIO (print "Done")

-- Even more dynamic example
c5 :: IO ()
c5 = runReaderT (loop =<< runC (th client)) (10::Int)
 where loop (Y x k) = (liftIO . print) (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
       loop Done    = (liftIO . print) "Done"

       -- cl, client, ay have to be rank-2 polymorphic in the mtl case
       client :: (MonadCo r m, MonadReader r m) => m ()
       client = ay >> ay
       ay :: (MonadCo r m, MonadReader r m) => m ()
       ay     = ask >>= yieldG

       th :: (forall m. (MonadCo r m, MonadReader Int m) => m ())
             -> ((MonadCo r m, MonadReader Int m) => m ())
       th cl = do
         cl
         v <- ask
         -- vv Rank2Types make things somewhat more awkward here
         if v > (20::Int) then cl else localLocal (+(5::Int)) cl
         if v > (20::Int) then return () else localLocal (+(10::Int)) (th cl)

-- Run the examples
examples :: [(String, IO ())]
examples = [ ("ex4_1_MTL", print ex4_1_MTL)
           , ("ex4_2_MTL", print ex4_2_MTL)
           , ("ex2r_MTL", print ex2r_MTL)
           , ("c3_MTL", c3_MTL)
           , ("c31_MTL", c31_MTL) ]

runTheExamples :: IO ()
runTheExamples = display examples
  where display = sequence_ . map (\(a, b) -> do
                                      putStrLn (a ++ ": ")
                                      _ <- b
                                      putStrLn "")
