module WriteToStore where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store

import Data.Newtype (unwrap, wrap)
import Data.String (trim)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

type WriteToStoreArgs =
  { originalFullPath :: Path
  , storeConfig :: StoreConfig
  , tags :: Array String
  }

type WriteToStoreRes =
  { stored :: Array Path 
  }

writeToStore :: WriteToStoreArgs -> Aff WriteToStoreRes
writeToStore args = do
  sha <- getShaOfPath args.originalFullPath
  let build = buildSource sha args.storeConfig 
  pure { stored: [] }

install :: DocSource -> Path -> Aff DocSource
install srcDoc destRoot = do
  let src = srcDoc.path
  let dest = append destRoot $ segment $ unwrap srcDoc.sha
  runScript ensureRun do
    src' <- asArg src
    dest' <- asArg dest
    addWords ["mv", src', dest']
  pure { sha: srcDoc.sha, path: dest }

getShaOfPath :: Path -> Aff Sha
getShaOfPath path = do
  uncleanSha <- runScript runAndCapture do
    path' <- asArg path
    addWords [ "cat" , path' ]
    addPipe
    addWords [ "sha1sum" ]
    addPipe
    addWords [ "awk", "'{ print $1 }'" ]
  pure $ wrap $ trim uncleanSha

getExt :: Path -> Aff Ext
getExt p = do
  pathLib <- liftEffect $ nodePath
  let format = parsePath pathLib $ show p
  pure $ wrap format.ext


