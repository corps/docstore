module Store where

import Paths
import Prelude
import ScriptBuilder

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (last)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (append)
import Data.String.Common (trim)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Node.Process (lookupEnv)
import Runner (ensureRun, runAndCapture)

newtype Sha = Sha String
derive instance shaNewtype :: Newtype Sha _
derive instance shaEq :: Eq Sha
derive instance shaOrd :: Ord Sha

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
        , tmpDir: segment "/tmp"
        }

  mkDir config.root
  mkDir config.allRoot

  pure config

type DocSource =
  { sha :: Sha
  , originalExt :: Ext
  , originalName :: Path
  , parentDir :: Path
  }

newtype GroupBySha r = GroupBySha { sha :: Sha | r}
instance groupByShaEq :: Eq (GroupBySha a) where
  eq (GroupBySha a) (GroupBySha b) = a.sha == b.sha

instance groupByShaOrd :: Ord (GroupBySha a) where
  compare (GroupBySha a) (GroupBySha b) = a.sha `compare` b.sha

readDocSource :: Path -> Aff DocSource
readDocSource parentDir = do
  checkExists parentDir

  originalName <- map trim $ runScript runAndCapture do
    origPath' <- asArg $ show $ docOriginalPath parentDir
    addWords ["cat", origPath']

  fmt <- liftEffect do
    pathLib <- nodePath
    pure $ parsePath pathLib originalName

  sha <- case last $ unwrap parentDir of
    Just sha -> pure sha
    Nothing -> throwError $ error "Cannot read docSource from empty path"

  pure $
    { parentDir: parentDir
    , originalName: segment fmt.name
    , originalExt: Ext fmt.ext
    , sha: Sha sha
    }

docFile :: DocSource -> Ext -> Path
docFile d ext = d.parentDir <> (segment $ unwrap d.sha) `addExt` ext

docOriginal :: DocSource -> Path
docOriginal d = d.parentDir <> d.originalName `addExt` d.originalExt

docOriginalPath :: Path -> Path
docOriginalPath p = p <> segment "orig.path"
