module Main where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store
import WriteToStore
import QueryStore

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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (class Semigroup)
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

-- openDoc :: String -> Aff Unit
-- openDoc pattern = do
--   storeBase <- liftEffect findStoreBase
--   ensureRun "open \"$1\"/$2" [originalsDir storeBase, pattern <> "*"]

-- logScript :: String
-- logScript = joinWith " | " script
--    where
--      script :: Array String
--      script = [ "ls -ltc \"$1\"", 
--                 "tail -n +2", 
--                 "head -n 20", 
--                 "awk -vpath=\"$1\"/ '{ print path$9 }'", 
--                 "xargs -L 1 bash -c 'for path; do stat -c \"%y %n\" $path; echo -n \"  \"; head -n 1 $path; done' bash"
--                ]

checkExists :: Path -> Aff Unit
checkExists path =
  runScript ensureRun do
    path' <- asArg path
    addWords ["[[", "-e", path', "]]"]

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
      fullPath <- runScript runAndCapture do
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
    traverse_ checkExists tagPaths
    storeConfig <- prepareStore
    queryStore (args storeConfig)

    where 
      tagPaths = segment <$> tags
      args :: StoreConfig -> QueryStoreArgs
      args storeConfig =
        { storeConfig: storeConfig
        , tagPaths: tagPaths
        , words: words
        }

-- app "query" text tags = do
--   launchAff_ do
--     storeBase <- liftEffect findStoreBase
--     let fullText = joinWith "\n" text
--     when (fullText /= "") do 
--       liftEffect $ log "By Text:"
--       ensureRun "grep -Ri -F \"$1\" \"$2\"" [fullText, ocrDir storeBase]

--     let fullTags = joinWith "\n" tags
--     when (fullTags /= "") do 
--       liftEffect $ log "By Tags:"
--       ensureRun "grep -Ri -F \"$1\" \"$2\"" [fullTags, tagsDir storeBase]

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
