module Plugin () where

import Network.WtsDvc.Client (createListener)
import Network.WtsDvc.Client.TH

initialize :: IO ()
initialize = createListener "haskell-wtsdvc-test" (pure Nothing)

declareEntryPoint 'initialize
