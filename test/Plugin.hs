module Plugin () where

import Foreign.C (CInt (..))
import System.IO (hFlush, stdout)

import Network.WtsDvc.Client (catchAllExceptions)

initialize :: IO CInt
initialize = catchAllExceptions $ do
    putStrLn "Haskell is running, and the build process is impeccable!"
    hFlush stdout

foreign export ccall "wts_hs_initialize" initialize :: IO CInt
