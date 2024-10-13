{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

import Bluefin.Coroutine (Coroutine, connectCoroutines, yieldCoroutine)
import Bluefin.Eff (Eff, runEff, (:>))
import Bluefin.Eff qualified as B (bracket)
import Bluefin.Exception qualified as B (throw, try)
import Bluefin.IO (IOE, effIO)
import Bluefin.Stream (Stream)
import Bluefin.Stream qualified as B (yield)
import Control.Exception qualified as E
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit (runConduit, (.|))
import Data.Conduit qualified as C
import Data.Foldable (for_)
import Pipes (Consumer, Producer, runEffect, (>->))
import Pipes qualified as P (await, yield)
import Pipes.Safe (SafeT, runSafeT)
import Pipes.Safe qualified as P (bracket, tryP)
import System.IO
import Control.Exception (bracket)


-- ghci> bracketExample
-- Opening file
-- Closing file
-- *** Exception: Too big
bracketExample :: Integer -> IO ()
bracketExample n =
  bracket
    (do
        putStrLn "Opening file"
        openFile "/usr/share/dict/words" ReadMode
    )
    (\handle -> do
        putStrLn "Closing file"
        hClose handle)
    (\handle -> do
        size <- hFileSize handle
        when (size >= n) (error "Too big")
        putStrLn "Not too big")

data MyEx = MyEx deriving (Show)

instance E.Exception MyEx

{-

Bluefin is better because

1. We don't leak the implementation detail of SafeT/ResourceT.

2. We don't have to worry about putting runSafeT/runResourceT in the
right place.

3. We don't have to wait until the end of the runSafeT/runResourceT
block to free the acquired resources.

These are three sides of the same coin

https://en.wikipedia.org/wiki/Cook_Islands_dollar#Coins

4. We don't have special-purpose tryP/tryC and bracket/bracketP, the
standard try and bracket work fine

-}

-- Thanks to
-- https://old.reddit.com/user/InThisStyle10s6p
pipes1 :: IO ()
pipes1 = runSafeT $ do
  runEffect $ do
    produce >-> consume
    produce >-> consume
  where
    produce :: Producer Int (SafeT IO) ()
    produce = do
      -- Special-purpose pipes bracket
      P.bracket
        (liftIO (putStrLn "Acquiring resource"))
        (\_ -> liftIO (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 .. 3] $ \i -> do
            P.yield i
        )

    consume :: (MonadIO m) => Consumer Int m ()
    consume = forever $ do
      r <- P.await
      liftIO (print r)

pipes2 :: IO ()
pipes2 = runSafeT $ do
  runEffect $ do
    produce >-> consume
    produce >-> consume
  where
    produce :: Producer Int (SafeT IO) ()
    produce = do
      -- Special-purpose pipes try
      void $ P.tryP @_ @MyEx $ do
        -- Special-purpose pipes bracket
        P.bracket
          (liftIO (putStrLn "Acquiring resource"))
          (\_ -> liftIO (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 .. 3] $ \i -> do
              when (i >= 3) $
                liftIO (E.throwIO MyEx)
              P.yield i
          )

    consume :: (MonadIO m) => Consumer Int m ()
    consume = forever $ do
      r <- P.await
      liftIO (print r)

conduit1 :: IO ()
conduit1 = runResourceT $ do
  runConduit $ do
    produce .| consume
    produce .| consume
  where
    produce :: C.ConduitT i Int (ResourceT IO) ()
    produce = do
      -- Special-purpose conduit bracket
      C.bracketP
        (liftIO (putStrLn "Acquiring resource"))
        (\_ -> liftIO (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 .. 3] $ \i -> do
            C.yield i
        )

    consume :: (MonadIO m) => C.ConduitT Int o m ()
    consume = do
      C.await >>= \case
        Nothing -> pure ()
        Just r -> do
          liftIO (print r)
          consume

conduit2 :: IO ()
conduit2 = runResourceT $ do
  runConduit $ do
    produce .| consume
    produce .| consume
  where
    produce :: C.ConduitT i Int (ResourceT IO) ()
    produce = do
      -- Special-purpose conduit try
      void $ C.tryC @_ @MyEx $ do
        -- Special-purpose conduit bracket
        C.bracketP
          (liftIO (putStrLn "Acquiring resource"))
          (\_ -> liftIO (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 .. 3] $ \i -> do
              when (i >= 3) (liftIO (E.throwIO MyEx))
              C.yield i
          )

    consume :: (MonadIO m) => C.ConduitT Int o m ()
    consume = do
      C.await >>= \case
        Nothing -> pure ()
        Just r -> do
          liftIO (print r)
          consume

bluefin1 :: IO ()
bluefin1 = runEff $ \io -> do
  connectCoroutines (consume io) (produce io)
  connectCoroutines (consume io) (produce io)
  where
    produce ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      () ->
      Stream Int e2 ->
      Eff es ()
    produce io () y = do
     -- General-purpose Bluefin bracket
      B.bracket
        (effIO io (putStrLn "Acquiring resource"))
        (\_ -> effIO io (putStrLn "Releasing resource"))
        ( \_ -> for_ [1 :: Int .. 3] $ \i -> do
            B.yield y i
        )

    consume ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      Coroutine () Int e2 ->
      Eff es ()
    consume io c = forever $ do
      r <- yieldCoroutine c ()
      effIO io (print r)

bluefin3 :: IO ()
bluefin3 = runEff $ \io -> do
  connectCoroutines (consume io) (produce io)
  connectCoroutines (consume io) (produce io)
  where
    produce ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      () ->
      Stream Int e2 ->
      Eff es ()
    produce io () y = do
        -- General-purpose Bluefin bracket
        B.bracket
          (effIO io (putStrLn "Acquiring resource"))
          (\_ -> effIO io (putStrLn "Releasing resource"))
          ( \_ -> for_ [1 :: Int .. 3] $ \i -> do
              B.yield y i
          )

    consume ::
      (e1 :> es, e2 :> es) =>
      IOE e1 ->
      Coroutine () Int e2 ->
      Eff es ()
    consume io c = forever $ do
      -- General-purpose Bluefin try
      void $ B.try $ \e -> do
        r <- yieldCoroutine c ()
        when (r >= 3) (B.throw e ())
        effIO io (print r)
