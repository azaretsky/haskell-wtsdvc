module Network.WtsDvc.Client (
    Channel (..),
    catchAllExceptions,
    createListener
) where

import Control.Concurrent (MVar, newMVar, swapMVar, withMVar)
import Control.Exception (SomeException, catch, displayException, onException)
import qualified Data.ByteString as B (ByteString, packCStringLen)
import qualified Data.ByteString.Unsafe as B (unsafeUseAsCStringLen)
import Data.Foldable (traverse_)
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

data Channel = Channel {submit :: B.ByteString -> IO (), close :: IO ()}

reportUnhandledException :: String -> SomeException -> IO ()
reportUnhandledException loc e = hPutStrLn stderr $ loc <> " unhandled exception: " <> displayException e

catchAllExceptions :: String -> IO () -> IO CInt
catchAllExceptions loc action =
    catch
        (action >> return 0)
        (\e -> reportUnhandledException loc e >> return (-1))

data WTSChannel

wrapChannel :: Ptr WTSChannel -> IO (ForeignPtr WTSChannel)
wrapChannel p = do
    c_refChannel p
    newForeignPtr channelFinalizer p

useChannel :: String -> (Ptr WTSChannel -> IO CInt) -> ForeignPtr WTSChannel -> IO ()
useChannel loc action f = throwIfNeg_ (const loc) $ do
    res <- withForeignPtr f action
    finalizeForeignPtr f
    return res

closeChannel :: ForeignPtr WTSChannel -> IO ()
closeChannel = useChannel "closeChannel" c_closeChannel

writeChannel :: B.ByteString -> ForeignPtr WTSChannel -> IO ()
writeChannel bytes = useChannel "writeChannel" $ \p ->
    B.unsafeUseAsCStringLen bytes $ \(bytesPtr, len) ->
        c_writeChannel p bytesPtr (fromIntegral len)

type ChannelHolder = MVar (Maybe (ForeignPtr WTSChannel))

holdChannel :: ForeignPtr WTSChannel -> IO ChannelHolder
holdChannel = newMVar . Just

extractChannel :: ChannelHolder -> IO (Maybe (ForeignPtr WTSChannel))
extractChannel m = withMVar m . traverse $ \f -> withForeignPtr f wrapChannel

consumeChannel :: ChannelHolder -> (ForeignPtr WTSChannel -> IO ()) -> IO ()
consumeChannel m action = swapMVar m Nothing >>= traverse_ action

releaseChannel :: ChannelHolder -> IO ()
releaseChannel m = consumeChannel m finalizeForeignPtr

type ListenerCallback = Channel -> IO (Maybe Channel)

createListener :: String -> ListenerCallback -> IO ()
createListener channelName listenerCallback =
    withCAString channelName $ \c_channelName -> do
        cbPtr <- newStablePtr listenerCallback
        throwIfNeg_ (const "createListener") $ c_createListener c_channelName cbPtr

newChannelConnection
    :: StablePtr ListenerCallback
    -> Ptr WTSChannel
    -> Ptr CInt
    -> Ptr (StablePtr Channel)
    -> IO CInt
newChannelConnection spListener p acceptPtr spCbPtr = catchAllExceptions "newChannelConnection" $ do
    listener <- deRefStablePtr spListener
    m <- wrapChannel p >>= holdChannel
    let output = Channel
            { submit = \bytes -> extractChannel m >>= maybe throwClosed (writeChannel bytes)
            , close = consumeChannel m closeChannel
            }
    maybeHandler <- listener output `onException` releaseChannel m
    accept <- case maybeHandler of
        Nothing -> do
            releaseChannel m
            return False
        Just channel -> do
            spCb <- newStablePtr channel{close = releaseChannel m >> close channel}
            poke spCbPtr spCb
            return True
    poke acceptPtr $ fromBool accept

throwClosed :: IO a
throwClosed = ioError $
    mkIOError illegalOperationErrorType "channelWrite" Nothing Nothing
    `ioeSetErrorString`
    "channel is closed"

dataReceived :: StablePtr Channel -> Ptr a -> Word32 -> IO CInt
dataReceived spCbPtr buf len = catchAllExceptions "dataReceived" $ do
    channel <- deRefStablePtr spCbPtr
    B.packCStringLen (castPtr buf, fromIntegral len) >>= submit channel

channelClosed :: StablePtr Channel -> IO CInt
channelClosed spCbPtr = catchAllExceptions "channelClosed" $ do
    deRefStablePtr spCbPtr >>= close

foreign export ccall "wts_hs_new_channel_connection" newChannelConnection
    :: StablePtr ListenerCallback
    -> Ptr WTSChannel
    -> Ptr CInt
    -> Ptr (StablePtr Channel)
    -> IO CInt

foreign export ccall "wts_hs_data_received" dataReceived
    :: StablePtr Channel
    -> Ptr a
    -> Word32
    -> IO CInt

foreign export ccall "wts_hs_closed" channelClosed
    :: StablePtr Channel
    -> IO CInt

foreign import ccall "wts_create_listener" c_createListener
    :: CString
    -> StablePtr ListenerCallback
    -> IO CInt

foreign import ccall "wts_ref_channel" c_refChannel
    :: Ptr WTSChannel
    -> IO ()

foreign import ccall "&wts_unref_channel" channelFinalizer
    :: FinalizerPtr WTSChannel

foreign import ccall "wts_write_channel" c_writeChannel
    :: Ptr WTSChannel
    -> Ptr a
    -> Word32
    -> IO CInt

foreign import ccall "wts_close_channel" c_closeChannel
    :: Ptr WTSChannel
    -> IO CInt
