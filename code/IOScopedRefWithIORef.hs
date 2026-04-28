{-# LANGUAGE GHC2021 #-}

module Main where

import Control.Concurrent qualified
import Control.Concurrent.Async qualified
import Control.Exception qualified
import Control.Monad (when)
import Data.IORef qualified

-- =========================================================
-- 1. IOScopedRef
-- =========================================================

newtype IOScopedRef a = MkIOScopedRef (Data.IORef.IORef a)

-- Like ReaderT's runReaderT
withIOScopedRef :: a -> (IOScopedRef a -> IO r) -> IO r
withIOScopedRef a k = do
  ref <- Data.IORef.newIORef a
  k (MkIOScopedRef ref)

-- Like ReaderT's ask
readIOScopedRef :: IOScopedRef a -> IO a
readIOScopedRef (MkIOScopedRef ref) = Data.IORef.readIORef ref

-- Like ReaderT's local
modifyIOScopedRefBad :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRefBad f (MkIOScopedRef ref) k = do
  orig <- Data.IORef.readIORef ref
  Data.IORef.modifyIORef ref f
  r <- k
  Data.IORef.writeIORef ref orig
  pure r

-- Like ReaderT's local
modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> IO r -> IO r
modifyIOScopedRef f (MkIOScopedRef ref) k =
  Control.Exception.bracket
    (Data.IORef.readIORef ref)
    (Data.IORef.writeIORef ref)
    ( \orig -> do
        Data.IORef.modifyIORef ref f
        k
    )

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
            putStrLn ("[" ++ show (lvl + cur) ++ "] " ++ msg),
          modifySeverity = \f action ->
            modifyIOScopedRefBad f ref action
        }

-- Logger example

getUser :: IO ()
getUser = pure ()

getData :: () -> IO ()
getData () = pure ()

isVip () = True

writeData () = pure ()

-- > loggerExample
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [0] Done
loggerExample :: IO ()
loggerExample = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id
  d <- modifySeverity logger modification $ do
    logMsg logger 0 "Getting data"
    getData user
  writeData d
  logMsg logger 0 "Done"

-- ghci> loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [11] Got exception
-- [10] Done
loggerExampleException :: IO ()
loggerExampleException = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  d <-
    Control.Exception.handle
      (\Exception -> logMsg logger 1 "Got exception")
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          Control.Exception.throw Exception
          getData user
      )
  writeData d
  logMsg logger 0 "Done"

loggerExampleConcurrently :: IO ()
loggerExampleConcurrently = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  -- ghci> loggerExampleConcurrently
  -- [1] Getting user
  -- [1] Is VIP: True
  -- [-90] Getting data
  -- [0] Done
  (d, ()) <- Control.Concurrent.Async.concurrently
    ( modifySeverity logger modification $ do
        logMsg logger 0 "Getting data"
        getData user
    )
    ( modifySeverity logger (subtract 100) $ do
        -- Some unimportant background processing
        Control.Concurrent.threadDelay 1000
    )
  writeData d
  logMsg logger 0 "Done"

-- =========================================================
-- 2. Server API
-- =========================================================

data Request = Request

data Response = Response

runServer :: (Request -> IO Response) -> IO ()
runServer k = do
  k Request
  pure ()

-- =========================================================
-- 3. Client code
-- =========================================================

data Exception = Exception deriving (Show)

instance Control.Exception.Exception Exception

doWork1 :: Request -> IO String
doWork1 _ = pure "Did work 1"

response :: String -> Response
response _ = Response

handle :: Logger -> Request -> IO Response
handle logger request = do
  logMsg logger 1 "handling request"

  t <-
    Control.Exception.handle
      ( \Exception -> do
          logMsg logger 1 "worker 1: failed"
          pure "Failed"
      )
      ( do
          -- We care more about logs from worker 1
          modifySeverity logger (+ 1) $ do
            logMsg logger 2 "worker 1: doing work"
            r <- doWork1 request
            Control.Exception.throw Exception
            logMsg logger 2 "worker 1: done work"
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
