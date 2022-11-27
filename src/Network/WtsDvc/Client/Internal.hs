module Network.WtsDvc.Client.Internal (
    catchAllExceptions
) where

import Control.Exception (SomeException, catch, displayException)
import Foreign.C (CInt)
import System.IO (hPutStrLn, stderr)

reportUnhandledException :: String -> SomeException -> IO ()
reportUnhandledException loc e = hPutStrLn stderr $ loc <> " unhandled exception: " <> displayException e

catchAllExceptions :: String -> IO () -> IO CInt
catchAllExceptions loc action =
    catch
        (action >> return 0)
        (\e -> reportUnhandledException loc e >> return (-1))
