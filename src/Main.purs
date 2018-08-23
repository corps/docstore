module Main where

import Paths
import Prelude
import QueryStore
import Runner
import ScriptBuilder
import Store
import WriteToStore

import Control.Alt ((<|>))
import Control.Applicative (class Applicative, unless, when)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Reader (Reader)
import Data.Array (head, length, replicate, take, uncons, zip)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (const)
import Data.List (toUnfoldable)
import Data.Map.Internal (values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (class Semigroup)
import Data.String (trim)
import Data.String.Common (joinWith, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (error, throwException)
import Node.Process (lookupEnv)
import Node.Yargs.Applicative (flag, rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Paths (PathFormat, formatPath, nodePath, parsePath)

newtype StoreBase = StoreBase String
derive instance storeBaseNewtype :: Newtype StoreBase _

app :: String -> Array String -> Array String -> Boolean -> Boolean -> Array String -> Effect Unit

-- Store procedure
app "store" tags names parent _ files = launchAff_ $ do
  unless parent $ traverse_ checkExists tagPaths
  storeConfig <- prepareStore
  traverse_ (go storeConfig) origAndNewNames

  where
    go :: StoreConfig -> Tuple String (Maybe String) -> Aff Unit
    go storeConfig origAndNew = do
      res <- argsForFile storeConfig origAndNew >>= writeToStore
      case head res of
        Just docStore -> liftEffect $ log $ unwrap docStore.sha
        otherwise -> pure unit

    argsForFile :: StoreConfig -> Tuple String (Maybe String) -> Aff WriteToStoreArgs
    argsForFile storeConfig (Tuple file newName) = do
      fullPath <- map trim $ runScript runAndCapture do
        path' <- asArg file
        addWords ["realpath", path']

      pure $ { originalFullPath: Path [fullPath]
      , storeConfig: storeConfig
      , tagPaths: tagPaths
      , p: parent
      , newName: newName
      }

    tagPaths = segment <$> tags

    namesWithFiles :: Array String
    namesWithFiles = take (length files) names

    namesWithoutFiles :: Array (Maybe String)
    namesWithoutFiles = replicate (length files - length namesWithFiles) Nothing

    newNames :: Array (Maybe String)
    newNames = (Just <$> namesWithFiles) <> namesWithoutFiles

    origAndNewNames = zip files newNames

-- query procedure
app "query" tags _ _ open words = do
  launchAff_ do
    storeConfig <- prepareStore
    let tagPaths = if tags /= [] then segment <$> tags else [storeConfig.allRoot]

    traverse_ checkExists tagPaths
    let args = { storeConfig: storeConfig
                , tagPaths: tagPaths
                , words: words
                }
    found <- map toUnfoldable $ map values $ queryStore args

    liftEffect $ log $ joinWith "\n\n" $
      joinWith "\n" <$> showQueryMatch <$> found

app _ _ _ _ _ _ = pure unit

main = do
  let setup = usage "$0 [--action store|log|query|open] [--tag x --tag y ...] [<file> <file2> ...]"
  runY setup $ app 
    <$> yarg "a" ["action"] (Just "the action to take, default: store") (Left "store") false 
    <*> yarg "t" ["tag"] (Just "tags to associate") (Left []) false
    <*> yarg "n" ["rename"] (Just "name to give stored files") (Left []) false
    <*> flag "p" ["parent"] Nothing
    <*> flag "o" ["open"] Nothing
    <*> rest


-- --action store|query
-- --tag dir
-- --rename n
-- --parent
-- --open
