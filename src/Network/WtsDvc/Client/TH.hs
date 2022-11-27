module Network.WtsDvc.Client.TH (declareEntryPoint, CInt (..)) where

import Foreign.C (CInt (..))
import Language.Haskell.TH
import Network.WtsDvc.Client.Internal (catchAllExceptions)

declareEntryPoint :: Name -> Q [Dec]
declareEntryPoint entry = do
    let exportName = mkName "wtsHsInitialize_th_generated"
    ty <- [t| IO CInt |]
    body <- [e| catchAllExceptions "wtsHsInitialize" $(varE entry) |]
    pure [
        SigD exportName ty
      , ValD (VarP exportName) (NormalB body) []
      , ForeignD $ ExportF CCall "wts_hs_initialize" exportName ty
      ]
