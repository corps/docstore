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
  , path :: Path
  }

buildSource :: Sha -> StoreConfig -> DocSource
buildSource sha config = { sha: sha, path: append config.tmpDir $ segment $ unwrap sha }

