{-# LANGUAGE GHC2021 #-}

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Stream
import Bluefin.System.IO hiding (Handle)
import Control.Monad
import System.IO (IOMode (WriteMode))
import Prelude hiding (log)

type Severity = Int

newtype Logger e =
  -- Log a message with a severity
  MkLogger {logImpl :: String -> Severity -> Eff e ()}

instance Handle Logger where
  mapHandle logger =
    MkLogger
      { logImpl = (fmap . fmap) useImpl (logImpl logger)
      }

log :: (e :> es) => Logger e -> String -> Severity -> Eff es ()
log = operationFrom logImpl

exampleWithLogger ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  Logger e2 ->
  Eff es ()
exampleWithLogger io logger = do
  effIO io (putStrLn "Started Logger example")
  log logger "Mild Logger message" 0
  log logger "Severe Logger message" 10
  effIO io (putStrLn "Ended Logger example")

exampleWithLogger' ::
  (e1 :> es, e2 :> es, IsLogger logger) =>
  IOE e1 ->
  logger e2 ->
  Eff es ()
exampleWithLogger' io logger' = do
  let logger = isLogger logger'
  effIO io (putStrLn "Started Logger example")
  log logger "Mild Logger message" 0
  log logger "Severe Logger message" 10
  effIO io (putStrLn "Ended Logger example")

withStdoutLogger ::
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
withStdoutLogger io k =
  useImplIn
    k
    MkLogger
      { logImpl =
          \msg sev ->
            effIO io (putStrLn (mkMsg msg sev))
      }
  where
    mkMsg msg sev =
      "Logger message: " ++ show sev ++ ": " ++ msg

runExampleWithLogger :: IO ()
runExampleWithLogger = runEff $ \io -> do
  -- "Instantiate" the Logger interface
  -- with a stdout logger
  withStdoutLogger io $ \logger -> do
    -- Use the Logger "instance"
    exampleWithLogger io logger

withLogAboveSeverityLogger ::
  (e1 :> es) =>
  Severity ->
  Logger e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
withLogAboveSeverityLogger minSev logger k = do
  useImplIn
    k
    MkLogger
      { logImpl = \msg sev -> do
          when (sev >= minSev) $ do
            log logger msg sev
      }

withLogAboveSeverityLogger' ::
  (e1 :> es, IsLogger h) =>
  Severity ->
  h e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
withLogAboveSeverityLogger' minSev logger' k = do
  useImplIn
    k
    MkLogger
      { logImpl = \msg sev -> do
          when (sev >= minSev) $ do
            log logger msg sev
      }
  where
    logger = isLogger logger'

runExampleWithLogAboveSeverityLogger :: IO ()
runExampleWithLogAboveSeverityLogger = runEff $ \io -> do
  -- Make the stdout logger
  withStdoutLogger io $ \logger -> do
    -- Only log messages of severity 5 and above
    withLogAboveSeverityLogger 5 logger $ \severeLogger -> do
      -- Run the example with the restricted logger
      exampleWithLogger io severeLogger

data FileLogger e = MkFileLogger
  { fileLoggerLogger :: Logger e,
    flushImpl :: Eff e ()
  }

instance Handle FileLogger where
  mapHandle fileLogger =
    MkFileLogger
      { fileLoggerLogger =
          mapHandle (fileLoggerLogger fileLogger),
        flushImpl =
          useImpl (flushImpl fileLogger)
      }

flush :: (e :> es) => FileLogger e -> Eff es ()
flush = operationFrom flushImpl

withFileLogger ::
  (e1 :> es) =>
  FilePath ->
  IOE e1 ->
  (forall e. FileLogger e -> Eff (e :& es) r) ->
  Eff es r
withFileLogger fp io k =
  -- Open a file for writing
  withFile io fp WriteMode $ \handle -> do
    -- Create the FileLogger
    useImplIn
      k
      MkFileLogger
        { fileLoggerLogger =
            MkLogger
              { logImpl =
                  \msg sev ->
                    -- Log to the open file
                    hPutStrLn handle (mkMsg msg sev)
              },
          flushImpl = do
            -- Diagnostic message just for the sake of
            -- the example
            effIO io (putStrLn "Flushing FileLogger")
            -- Flush writes to the file
            hFlush handle
        }
  where
    mkMsg msg sev =
      "FileLogger message: " ++ show sev ++ ": " ++ msg

exampleWithFileLogger ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  FileLogger e2 ->
  Eff es ()
exampleWithFileLogger io fileLogger = do
  -- Create a Logger from the FileLogger
  let logger = fileLoggerLogger fileLogger

  effIO io (putStrLn "Started FileLogger example")
  -- Log to the FileLogger
  log logger "Mild FileLogger message" 0

  withLogAboveSeverityLogger 5 logger $ \severeLogger -> do
    exampleWithLogger io severeLogger

  -- Flush the FileLogger
  flush fileLogger

  log logger "Severe FileLogger message" 10
  effIO io (putStrLn "Ended Logger example")

exampleWithFileLogger' ::
  (e1 :> es, e2 :> es) =>
  IOE e1 ->
  FileLogger e2 ->
  Eff es ()
exampleWithFileLogger' io fileLogger = do
  -- Create a Logger from the FileLogger
  let logger = fileLoggerLogger fileLogger

  effIO io (putStrLn "Started FileLogger example")
  -- Log to the FileLogger
  log logger "Mild FileLogger message" 0

  withLogAboveSeverityLogger' 5 fileLogger $ \severeLogger -> do
    exampleWithLogger io severeLogger

  -- Flush the FileLogger
  flush fileLogger

  log logger "Severe FileLogger message" 10
  effIO io (putStrLn "Ended Logger example")

runExampleWithFileLogger :: IO ()
runExampleWithFileLogger = runEff $ \io -> do
  -- Create the FileLogger
  withFileLogger "/dev/stdout" io $ \fileLogger ->
    -- Use the FileLogger
    exampleWithFileLogger io fileLogger

class IsLogger h where
  isLogger :: h e -> Logger e

instance IsLogger FileLogger where
  isLogger = fileLoggerLogger

streamLogger ::
  (e1 :> es) =>
  Stream String e1 ->
  (forall e. Logger e -> Eff (e :& es) r) ->
  Eff es r
streamLogger stream k =
  useImplIn
    k
    MkLogger
      { logImpl = \msg sev ->
          yield stream (mkMsg msg sev)
      }
  where
    mkMsg msg sev =
      "streamLogger message: " ++ show sev ++ ": " ++ msg

runExampleStreamLogger :: IO ()
runExampleStreamLogger = runEff $ \io -> do
  forEach
    ( \stream -> streamLogger stream $ \logger -> do
        exampleWithLogger io logger
    )
    ( \logMsg -> do
        effIO io (putStrLn logMsg)
    )
