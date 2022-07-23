module Network.WtsDvc.Client (
    Channel,
    catchAllExceptions,
    chWrite,
    chClose,
    createListener
) where

import Control.Concurrent (MVar, newMVar, swapMVar, withMVar)
import Control.Monad (when)
import Control.Exception (SomeException, catch, displayException, finally, onException)
import qualified Data.ByteString as B (ByteString, packCStringLen)
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
import Data.Word (Word32)
import Foreign (
    FinalizerPtr,
    ForeignPtr,
    Ptr,
    StablePtr,
    castPtr,
    deRefStablePtr,
    finalizeForeignPtr,
    fromBool,
    newForeignPtr,
    newStablePtr,
    nullPtr,
    poke,
    throwIfNeg_,
    withForeignPtr
  )
import Foreign.C (CInt (..), CString, withCAString)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (illegalOperationErrorType, ioeSetErrorString, mkIOError)

reportUnhandledException :: String -> SomeException -> IO ()
reportUnhandledException loc e = hPutStrLn stderr $ loc <> ": unhandled exception " <> displayException e

catchAllExceptions :: String -> IO () -> IO CInt
catchAllExceptions loc action =
    catch
        (action >> return 0)
        (\e -> reportUnhandledException loc e >> return (-1))

newtype Channel = Channel (MVar (Maybe (ForeignPtr Channel)))

wrapChannel :: Ptr Channel -> IO Channel
wrapChannel p = do
    c_refChannel p
    f <- newForeignPtr channelFinalizer p
    Channel <$> newMVar (Just f)

chWrite :: Channel -> B.ByteString -> IO ()
chWrite (Channel m) bytes = do
    p <- withMVar m $ \case
        Nothing -> return nullPtr
        Just f -> withForeignPtr f $ \p -> do
            c_refChannel p
            return p
    when (p == nullPtr) $ ioError $
        mkIOError illegalOperationErrorType "chWrite" Nothing Nothing
        `ioeSetErrorString`
        "channel is closed"
    throwIfNeg_ (const "chWrite") $ finally
        (B.unsafeUseAsCStringLen bytes $ \(bytesPtr, len) ->
            c_writeChannel p bytesPtr (fromIntegral len))
        (c_unrefChannel p)

chClose :: Channel -> IO ()
chClose (Channel m) = do
    c <- swapMVar m Nothing
    case c of
        Nothing -> return ()
        Just f -> throwIfNeg_ (const "chClose") $ do
            res <- withForeignPtr f c_closeChannel
            finalizeForeignPtr f
            return res

type ChannelCallback = (B.ByteString -> IO (), IO ())
type ListenerCallback = Channel -> IO (Maybe ChannelCallback)

createListener :: String -> ListenerCallback -> IO ()
createListener channelName listenerCallback =
    withCAString channelName $ \c_channelName -> do
        cbPtr <- newStablePtr listenerCallback
        throwIfNeg_ (const "createListener") $ c_createListener c_channelName cbPtr

newChannelConnection
    :: StablePtr ListenerCallback
    -> Ptr Channel
    -> Ptr CInt
    -> Ptr (StablePtr ChannelCallback)
    -> IO CInt
newChannelConnection spListener p acceptPtr spCbPtr = catchAllExceptions "newChannelConnection" $ do
    listener <- deRefStablePtr spListener
    channel@(Channel m) <- wrapChannel p
    let releaseChannel = swapMVar m Nothing >>= maybe (return ()) finalizeForeignPtr
    maybeHandler <- listener channel `onException` releaseChannel
    accept <- case maybeHandler of
        Nothing -> do
            releaseChannel
            return False
        Just (cbData, cbClosed) -> do
            spCb <- newStablePtr (cbData, releaseChannel >> cbClosed)
            poke spCbPtr spCb
            return True
    poke acceptPtr $ fromBool accept

dataReceived :: StablePtr ChannelCallback -> Ptr a -> Word32 -> IO CInt
dataReceived spCbPtr buf len = catchAllExceptions "dataReceived" $ do
    (cbData, _) <- deRefStablePtr spCbPtr
    bytes <- B.packCStringLen (castPtr buf, fromIntegral len)
    cbData bytes

channelClosed :: StablePtr ChannelCallback -> IO CInt
channelClosed spCbPtr = catchAllExceptions "channelClosed" $ do
    (_, cbClosed) <- deRefStablePtr spCbPtr
    cbClosed

foreign export ccall "wts_hs_new_channel_connection" newChannelConnection
    :: StablePtr ListenerCallback
    -> Ptr Channel
    -> Ptr CInt
    -> Ptr (StablePtr ChannelCallback)
    -> IO CInt

foreign export ccall "wts_hs_data_received" dataReceived
    :: StablePtr ChannelCallback
    -> Ptr a
    -> Word32
    -> IO CInt

foreign export ccall "wts_hs_closed" channelClosed
    :: StablePtr ChannelCallback
    -> IO CInt

foreign import ccall "wts_create_listener" c_createListener
    :: CString
    -> StablePtr ListenerCallback
    -> IO CInt

foreign import ccall "wts_ref_channel" c_refChannel
    :: Ptr Channel
    -> IO ()

foreign import ccall "wts_unref_channel" c_unrefChannel
    :: Ptr Channel
    -> IO ()

foreign import ccall "wts_write_channel" c_writeChannel
    :: Ptr Channel
    -> Ptr a
    -> Word32
    -> IO CInt

foreign import ccall "wts_close_channel" c_closeChannel
    :: Ptr Channel
    -> IO CInt

foreign import ccall "&wts_unref_channel" channelFinalizer
    :: FinalizerPtr Channel
