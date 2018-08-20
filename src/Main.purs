module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Applicative (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (State)
import Control.Parallel.Class (sequential)
import Data.Array (foldM)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show (show)
import Data.String.Common (trim)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error, error)
import Node.Process (lookupEnv)
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Paths (nodePath, parsePath)
import Processes (SpawnOptions, defaultSpawnOptions, spawn)

runBashCaptureOut :: SpawnOptions -> String -> Array String -> Aff String
runBashCaptureOut options cmd args = do
  liftEffect $ log $ cmd <> " " <> show args
  result <- spawn "bash" (["-c", cmd, "--"] <> args) options
  when (result.code /= 0) do
    throwError $ error $ "command failed '" <> cmd <> "' stderr: \n" <> result.stderr
  pure result.stdout

checkBashSuccess :: SpawnOptions -> String -> Array String -> Aff Boolean
checkBashSuccess options cmd args = do
  liftEffect $ log $ cmd <> " " <> show args
  result <- spawn cmd args options { shell = Just "/bin/bash" }
  pure $ result.code == 0

findDropboxHome :: Effect String
findDropboxHome = do
  winhome <- lookupEnv "WINHOME" 
  home <- lookupEnv "HOME"
  pure $ (fromMaybe "" (winhome <|> home)) <> "/Dropbox"

app :: Array String -> Array String -> Effect Unit
app [] _ = pure unit
app files tags = do
  dropboxHome <- findDropboxHome
  launchAff_ do
    traverse_ (processFile dropboxHome tags >=> liftEffect <<< log) files

processFile :: String -> Array String -> String -> Aff String
processFile dropboxHome tags file = do
  pathLib <- liftEffect $ nodePath
  let run = runBashCaptureOut defaultSpawnOptions
  let pathFormat = parsePath pathLib file
  uncleanSha <- run "cat \"$1\" | sha1sum | awk '{ print $1 }'" [file]
  let sha = trim uncleanSha
  let storedBase = sha <> pathFormat.ext
  let storedPath = dropboxHome <> "/Docs/originals/" <> storedBase
  _ <- run "mkdir -p \"$1\"" [dropboxHome <> "/Docs/originals/"]
  _ <- run "cp \"$1\" \"$2\"" [file, storedPath]
  pure storedBase

main = do
  let setup = usage "$0 [--tag x [--tag y]] <file> [<file2>...]"
  runY setup $ app 
    <$> rest
    <*> yarg "t" ["tag"] Nothing (Left []) false
