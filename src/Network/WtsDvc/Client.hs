module Network.WtsDvc.Client (
    ChannelHolder,
    catchAllExceptions,
    chWrite,
    chClose,
    createListener
) where

import Control.Concurrent (MVar, newMVar, swapMVar, withMVar)
import Control.Exception (SomeException, catch, displayException, onException)
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
    poke,
    throwIfNeg_,
    withForeignPtr
  )
import Foreign.C (CInt (..), CString, withCAString)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (illegalOperationErrorType, ioeSetErrorString, mkIOError)

reportUnhandledException :: String -> SomeException -> IO ()
reportUnhandledException loc e = hPutStrLn stderr $ loc <> " unhandled exception: " <> displayException e

catchAllExceptions :: String -> IO () -> IO CInt
catchAllExceptions loc action =
    catch
        (action >> return 0)
        (\e -> reportUnhandledException loc e >> return (-1))

data Channel

wrapChannel :: Ptr Channel -> IO (ForeignPtr Channel)
wrapChannel p = do
    c_refChannel p
    newForeignPtr channelFinalizer p

refChannel :: ForeignPtr Channel -> IO (ForeignPtr Channel)
refChannel f = withForeignPtr f wrapChannel

useChannel :: String -> ForeignPtr Channel -> (Ptr Channel -> IO CInt) -> IO ()
useChannel loc f action = throwIfNeg_ (const loc) $ do
    res <- withForeignPtr f action
    finalizeForeignPtr f
    return res

closeChannel :: ForeignPtr Channel -> IO ()
closeChannel f = useChannel "closeChannel" f c_closeChannel

writeChannel :: ForeignPtr Channel -> Ptr a -> Int -> IO ()
writeChannel f bytes len = useChannel "writeChannel" f $
    \p -> c_writeChannel p bytes (fromIntegral len)

type ChannelHolder = MVar (Maybe (ForeignPtr Channel))

holdChannel :: ForeignPtr Channel -> IO ChannelHolder
holdChannel = newMVar . Just

consumeChannel :: ChannelHolder -> (ForeignPtr Channel -> IO ()) -> IO ()
consumeChannel m action = swapMVar m Nothing >>= maybe (return ()) action

chWrite :: ChannelHolder -> B.ByteString -> IO ()
chWrite m bytes = do
    channelState <- withMVar m $ \case
        Nothing -> return Nothing
        Just f -> Just <$> refChannel f
    case channelState of
        Nothing -> ioError $
            mkIOError illegalOperationErrorType "chWrite" Nothing Nothing
            `ioeSetErrorString`
            "channel is closed"
        Just f -> B.unsafeUseAsCStringLen bytes $ uncurry $ writeChannel f

chClose :: ChannelHolder -> IO ()
chClose m = consumeChannel m closeChannel

releaseChannel :: ChannelHolder -> IO ()
releaseChannel m = consumeChannel m finalizeForeignPtr

type ChannelCallback = (B.ByteString -> IO (), IO ())
type ListenerCallback = ChannelHolder -> IO (Maybe ChannelCallback)

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
    m <- wrapChannel p >>= holdChannel
    maybeHandler <- listener m `onException` releaseChannel m
    accept <- case maybeHandler of
        Nothing -> do
            releaseChannel m
            return False
        Just (cbData, cbClosed) -> do
            spCb <- newStablePtr (cbData, releaseChannel m >> cbClosed)
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

foreign import ccall "&wts_unref_channel" channelFinalizer
    :: FinalizerPtr Channel

foreign import ccall "wts_write_channel" c_writeChannel
    :: Ptr Channel
    -> Ptr a
    -> Word32
    -> IO CInt

foreign import ccall "wts_close_channel" c_closeChannel
    :: Ptr Channel
    -> IO CInt
