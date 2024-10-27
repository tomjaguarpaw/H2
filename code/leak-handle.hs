import System.IO
import Bluefin.Eff
import Bluefin.System.IO
import Prelude

useHandle :: IO ()
useHandle = do
  System.IO.withFile "/tmp/message" WriteMode $ \handle -> do
    System.IO.hPutStrLn handle "Hello!"

-- handle has been closed by the time we get here

useHandleException :: IO ()
useHandleException = do
  System.IO.withFile "/tmp/message" WriteMode $ \handle -> do
    System.IO.hPutStrLn handle "Hello!"
    error "Oh dear!"

-- handle has been closed by the time we get here,
-- even though there was an exception

-- ghci> leakHandle

-- *** Exception: /tmp/output-file: hPutStr: illegal operation (handle is closed)

leakHandle :: IO ()
leakHandle = do
  handle <-
    System.IO.withFile "/tmp/output-file" WriteMode $ \handle -> do
      pure handle

  System.IO.hPutStrLn handle "Still open?"

-- /home/tom/H2/code/logger.hs:208:10: error:
--    • Couldn't match type ‘e0’ with ‘e1’
--      Expected: Bluefin.Internal.System.IO.Handle e0
--        Actual: Bluefin.Internal.System.IO.Handle e1
--    • because type variable ‘e1’ would escape its scope

leakHandleBf :: IO ()
leakHandleBf = runEff $ \io -> do
  handle <-
    Bluefin.System.IO.withFile
      io
      "/tmp/output-file"
      WriteMode
      $ \handle -> do
        pure handle

  Bluefin.System.IO.hPutStrLn handle "Still open?"
