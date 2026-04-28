{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Exception qualified
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans qualified
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Vault.Strict qualified as Vault
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async qualified
import UnliftIO.Concurrent qualified
import UnliftIO.Exception qualified
import UnliftIO.MVar qualified
import Prelude hiding (lookup)

vaultExample :: IO ()
vaultExample = do
  -- Create an empty vault
  let v0 = Vault.empty

  -- Create a key
  k0 <- Vault.newKey

  -- Look up the key in the vault
  let r1 = Vault.lookup k0 v0
  -- We haven't inserted anything into the vault at this key so the
  -- result is
  --
  -- Nothing
  print r1

  -- Insert something into the vault at key k0
  let v1 = Vault.insert k0 "Hello" v0

  -- Look up key k1 in the vault
  let r2 = Vault.lookup k0 v1
  -- Just "Hello"
  print r2

  -- Create another key
  k1 <- Vault.newKey

  -- Look up key k1 in the vault
  let r3 = Vault.lookup k1 v1
  -- We haven't inserted anything into the vault at this key so the
  -- result is
  --
  -- Nothing
  print r3

  -- Insert something into the vault at key k1
  let v2 = Vault.insert k1 True v1
  -- Look up each key in the vault. The result is
  --
  -- (Just "Hello", Just True)
  let r4 = (Vault.lookup k0 v2, Vault.lookup k1 v2)
  print r4

  -- Adjust (modify) the value stored at key k0
  let v3 = Vault.adjust (++ " world") k0 v2
  -- We get the modified value at k0 and the unchanged value at k1
  --
  -- (Just "Hello world",Just True)
  let r5 = (Vault.lookup k0 v3, Vault.lookup k1 v3)
  print r5

-- Constructor kept hidden
newtype IOScopedRef a = MkIOScopedRef (Vault.Key a)

-- Constructor kept hidden
newtype NewIO a = MkNewIO {unNewIO :: ReaderT Vault.Vault IO a}
  --  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

withIOScopedRef :: a -> (IOScopedRef a -> NewIO r) -> NewIO r
withIOScopedRef a body = MkNewIO $ do
  key <- Control.Monad.Trans.lift Vault.newKey
  Reader.local (Vault.insert key a) $
    unNewIO (body (MkIOScopedRef key))

readIOScopedRef :: IOScopedRef a -> NewIO a
readIOScopedRef (MkIOScopedRef key) = MkNewIO $ do
  vault <- Reader.ask
  case Vault.lookup key vault of
    Nothing -> error "IOScopedRef value not in scope"
    Just a -> pure a

modifyIOScopedRef :: (a -> a) -> IOScopedRef a -> NewIO r -> NewIO r
modifyIOScopedRef f (MkIOScopedRef key) (MkNewIO body) =
  MkNewIO (Reader.local (Vault.adjust f key) body)

runNewIO :: NewIO a -> IO a
runNewIO (MkNewIO io) = Reader.runReaderT io Vault.empty

--

type Severity = Int

data Logger = Logger
  { logMsg ::
      Severity ->
      String ->
      NewIO (),
    modifySeverity ::
      forall a.
      (Severity -> Severity) ->
      NewIO a ->
      NewIO a
  }

withStdoutLogger :: Severity -> (Logger -> NewIO r) -> NewIO r
withStdoutLogger initial k =
  withIOScopedRef initial $ \ref -> do
    k
      Logger
        { logMsg = \lvl msg -> do
            cur <- readIOScopedRef ref
            Control.Monad.Trans.liftIO
              (putStrLn ("[" ++ show (lvl + cur) ++ "] " ++ msg)),
          modifySeverity = \f action ->
            modifyIOScopedRef f ref action
        }

-- ghci> runNewIO loggerExampleException
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [1] Got exception
-- [0] Done
loggerExampleException :: NewIO ()
loggerExampleException = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  d <-
    UnliftIO.Exception.handle
      (\Exception -> logMsg logger 1 "Got exception")
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          _ <- UnliftIO.Exception.throwIO Exception
          getData user
      )
  writeData d
  logMsg logger 0 "Done"

-- ghci> runNewIO loggerExampleConcurrently
-- [1] Getting user
-- [1] Is VIP: True
-- [10] Getting data
-- [0] Done
loggerExampleConcurrently :: NewIO ()
loggerExampleConcurrently = withStdoutLogger 0 $ \logger -> do
  logMsg logger 1 "Getting user"
  user <- getUser
  logMsg logger 1 ("Is VIP: " <> show (isVip user))
  let modification = if isVip user then (+ 10) else id

  (d, ()) <-
    UnliftIO.Async.concurrently
      ( modifySeverity logger modification $ do
          logMsg logger 0 "Getting data"
          getData user
      )
      ( -- Do some unimportant background processing
        modifySeverity logger (subtract 100) $ do
          UnliftIO.Concurrent.threadDelay 1000
      )
  writeData d
  logMsg logger 0 "Done"

--

newIOScopedRef :: a -> NewIO (IOScopedRef a)
newIOScopedRef a = withIOScopedRef a pure

escape :: NewIO ()
escape = do
  ref <- newIOScopedRef ()
  readIOScopedRef ref

inAThread :: NewIO a -> NewIO a
inAThread = id

-- ghci> runNewIO badShare
-- \*** Exception: IOScopedRef value not in scope
badShare :: NewIO ((), String)
badShare = do
  mvar <- UnliftIO.MVar.newEmptyMVar
  UnliftIO.Async.concurrently
    ( withIOScopedRef "Hello" $ \ref -> do
        UnliftIO.MVar.putMVar mvar ref
        UnliftIO.Concurrent.threadDelay 1000000000
    )
    ( do
        ref' <- UnliftIO.MVar.takeMVar mvar
        readIOScopedRef ref'
    )

-- ghci> runNewIO goodShare
-- ((),"Hello")
goodShare :: NewIO ((), String)
goodShare = do
  mvar <- UnliftIO.MVar.newEmptyMVar
  withIOScopedRef "Hello" $ \ref -> do
    UnliftIO.Async.concurrently
      (UnliftIO.MVar.putMVar mvar ref)
      ( do
          ref' <- UnliftIO.MVar.takeMVar mvar
          readIOScopedRef ref'
      )

confusingShare :: NewIO ()
confusingShare = do
  mvar <- UnliftIO.MVar.newEmptyMVar
  UnliftIO.Async.concurrently_
    ( withIOScopedRef "Hello" $ \ref0 -> do
        UnliftIO.MVar.putMVar mvar ref0
        ref2 <- UnliftIO.MVar.takeMVar mvar
        _ <- readIOScopedRef ref2
        UnliftIO.Concurrent.threadDelay 1000000000
    )
    ( do
        ref1 <- UnliftIO.MVar.takeMVar mvar
        _ <- readIOScopedRef ref1
        modifyIOScopedRef (const "Bye") ref1 $ do
          UnliftIO.MVar.putMVar mvar ref1
          UnliftIO.Concurrent.threadDelay 1000000000
    )

-- Exception

data Exception = Exception deriving (Show)

instance Control.Exception.Exception Exception

-- Primitives

getUser :: NewIO ()
getUser = pure ()

getData :: () -> NewIO ()
getData () = pure ()

isVip :: () -> Bool
isVip () = True

writeData :: () -> NewIO ()
writeData () = pure ()

-- Higher order

type Query = ()

type Result = ()

getConnection :: IO ()
getConnection = pure ()

prepare :: Query -> IO ()
prepare _ = pure ()

runPrepared :: () -> () -> IO Result
runPrepared _ _ = pure ()

runQuery :: Query -> (forall a. String -> IO a -> IO a) -> IO Result
runQuery query label = do
  (conn, preparedQuery) <-
    UnliftIO.Async.concurrently
      ( label "obtain connection" $ do
          getConnection
      )
      ( label "preparing query" $ do
          prepare query
      )
  label "running query" $ do
    runPrepared conn preparedQuery
