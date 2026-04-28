{-# LANGUAGE GHC2021 #-}

module Main where

import Control.Concurrent.Async qualified as Async
import Control.Monad (when)

-- =========================================================
-- 1. IOScopedRef
-- =========================================================

data IOScopedRef a

-- Like ReaderT's runReaderT
withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r
withIOScopedRef = error "Left unimplemented"

-- Like ReaderT's ask
readIOScopedRef :: IOScopedRef a -> IO a
readIOScopedRef = error "Left unimplemented"

-- Like ReaderT's local
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRef = error "modifyIOScopedRef"

-- =========================================================
-- 1. Logger library
-- =========================================================

type Severity = Int

data Logger = Logger
  { logMsg ::
      Severity ->
      String ->
      IO (),
    modifySeverity ::
      forall a.
      (Severity -> Severity) ->
      IO a ->
      IO a
  }

withStdoutLogger :: Severity -> (Logger -> IO r) -> IO r
withStdoutLogger initial k =
  withIOScopedRef initial $ \ref -> do
    k
      Logger
        { logMsg = \lvl msg -> do
            cur <- readIOScopedRef ref
            when (lvl >= cur) $ do
              putStrLn ("[" ++ show lvl ++ "] " ++ msg),
          modifySeverity = \f action ->
            modifyIOScopedRef f ref action
        }

-- =========================================================
-- 2. Server API
-- =========================================================

data Request

data Response

runServer :: (Request -> IO Response) -> IO ()
runServer = error "Left unimplemented"

-- =========================================================
-- 3. Client code
-- =========================================================

doWork1 :: Request -> IO String
doWork1 = error "Left unimplemented"

doWork2 :: Request -> IO Int
doWork2 = error "Left unimplemented"

response :: (String, Int) -> Response
response = error "Left unimplemented"

handle :: Logger -> Request -> IO Response
handle logger request = do
  logMsg logger 1 "handling request"

  t <-
    Async.concurrently
      ( -- We care more about logs from worker 1
        modifySeverity logger (+ 1) $ do
          logMsg logger 2 "worker 1: doing work"
          r <- doWork1 request
          logMsg logger 2 "worker 1: done work"
          pure r
      )
      ( -- We care less about logs from worker 2
        modifySeverity logger (subtract 1) $ do
          logMsg logger 2 "worker 2: doing work"
          r <- doWork2 request
          logMsg logger 2 "worker 2: done work"
          pure r
      )

  logMsg logger 1 "done handling request"
  pure (response t)

-- =========================================================
-- 4. main
-- =========================================================

main :: IO ()
main =
  withStdoutLogger 1 $ \logger ->
    runServer (handle logger)
