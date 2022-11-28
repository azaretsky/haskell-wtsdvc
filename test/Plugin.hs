module Plugin () where

import Network.WtsDvc.Client (createListener, logUserMessage)
import Network.WtsDvc.Client.TH

initialize :: IO ()
initialize = createListener "haskell-wtsdvc-test" $ \out -> do
    logUserMessage "rejecting incoming connection"
    return Nothing

declareEntryPoint 'initialize
