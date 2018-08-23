module QueryStore where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store

import Effect.Aff (Aff)

type QueryStoreArgs =
  { storeConfig :: StoreConfig
  , tagPaths :: Array Path
  , words :: Array String
  }

type QueryStoreRes = Array DocSource

queryStore :: QueryStoreArgs -> Aff QueryStoreRes
queryStore args =
  pure []
