module Network.WtsDvc.Client.Internal (
    catchAllExceptions,
    logUserMessage
) where

import Control.Exception (SomeException, catch, displayException)
import Foreign.C (CInt, CString, withCString)

reportUnhandledException :: String -> SomeException -> IO ()
reportUnhandledException loc e = logUserMessage $ loc <> " unhandled exception: " <> displayException e

catchAllExceptions :: String -> IO () -> IO CInt
catchAllExceptions loc action =
    catch
        (action >> return 0)
        (\e -> reportUnhandledException loc e >> return (-1))

logUserMessage :: String -> IO ()
logUserMessage message = withCString message c_logUserMessage

foreign import ccall "log_user_message" c_logUserMessage :: CString -> IO ()
