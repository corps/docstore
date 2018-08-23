module Store where

import Paths
import Prelude
import ScriptBuilder

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (append)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Node.Process (lookupEnv)
import Runner (ensureRun)

newtype Sha = Sha String
derive instance shaNewtype :: Newtype Sha _

type StoreConfig =
  { root :: Path
  , allRoot :: Path
  , tmpDir :: Path
  }

prepareStore :: Aff StoreConfig
prepareStore = do
  root <- liftEffect do
    winhome <- lookupEnv "WINHOME"
    home <- lookupEnv "HOME"
    case winhome <|> home of
      Just trueHome -> pure $ Path $ [trueHome , "Dropbox/store"]
      Nothing -> throwException $ error "Could not determine Dropbox root"

  let config :: StoreConfig
      config =
        { root: root
        , allRoot: root <> segment "all"
        , tmpDir: root <> segment "tmp"
        }

  ensureRun "mkdir -p \"$1\"" [show config.root]
  ensureRun "mkdir -p \"$1\"" [show config.allRoot]

  pure config

type DocSource =
  { sha :: Sha
  , originalExt :: Ext
  , originalName :: Path
  , parentDir :: Path
  }

readDocSource :: Path -> Aff DocSource
readDocSource parentDir =
  pure $
    { parentDir: parentDir
    , originalName: originalName
    , originalExt: fmt.ext
    , sha: sha
    }

docFile :: DocSource -> Ext -> Path
docFile d ext = d.parentDir <> (segment $ unwrap d.sha) `addExt` ext

docOriginal :: DocSource -> Path
docOriginal d = d.parentDir <> d.originalName `addExt` d.originalExt

