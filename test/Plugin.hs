module Plugin () where

import System.IO (hFlush, stdout)

import Network.WtsDvc.Client (createListener)
import Network.WtsDvc.Client.TH

initialize :: IO ()
initialize = createListener "haskell-wtsdvc-test" $ \out -> do
    putStrLn "rejecting incoming connection"
    hFlush stdout
    return Nothing

declareEntryPoint 'initialize
