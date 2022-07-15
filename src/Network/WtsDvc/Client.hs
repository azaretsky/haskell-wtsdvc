module Network.WtsDvc.Client (catchAllExceptions) where

import Control.Exception (catch, displayException, SomeException)
import Foreign.C (CInt (..))
import System.IO (hPutStrLn, stderr)

reportUnhandledException :: SomeException -> IO ()
reportUnhandledException e = hPutStrLn stderr $ "unhandled exception: " <> displayException e

catchAllExceptions :: IO () -> IO CInt
catchAllExceptions action =
    catch
        (action >> return 1)
        (\e -> reportUnhandledException e >> return 0)
