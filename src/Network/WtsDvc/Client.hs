module Network.WtsDvc.Client (
    catchAllExceptions,
    createListener
) where

import Control.Monad (when)
import Control.Exception (catch, displayException, SomeException)
import qualified Data.ByteString as B (ByteString)
import Data.Int (Int32)
import Foreign (StablePtr, newStablePtr)
import Foreign.C (CInt, CString, withCAString)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

reportUnhandledException :: SomeException -> IO ()
reportUnhandledException e = hPutStrLn stderr $ "unhandled exception: " <> displayException e

catchAllExceptions :: IO () -> IO CInt
catchAllExceptions action =
    catch
        (action >> return 1)
        (\e -> reportUnhandledException e >> return 0)

type ChannelHandler = IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()

createListener :: String -> IO (Maybe ChannelHandler) -> IO ()
createListener channelName listenerCallback =
    withCAString channelName $ \c_channelName -> do
        cbPtr <- newStablePtr listenerCallback
        failOnHRESULT "createListener" $ c_createListener c_channelName cbPtr

type HRESULT = Int32

failOnHRESULT :: String -> IO HRESULT -> IO ()
failOnHRESULT location action = do
    r <- action
    when (r < 0) $ fail $ printf "%s failed 0x%08x" location r

foreign import ccall "wts_create_listener" c_createListener :: CString -> StablePtr (IO (Maybe ChannelHandler)) -> IO HRESULT
