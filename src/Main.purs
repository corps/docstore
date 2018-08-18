module Main where

import Prelude

import Control.Applicative (pure)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (readString)
import Node.Yargs.Applicative (flag, rest, runY, yarg)
import Node.Yargs.Setup (usage)

app :: Array String -> Array String -> Effect Unit
app [] _ = pure unit
app files tags = do
  logShow $ "Hi!" <> show files <> show tags

newtype FileProcessConfig = FileProcessConfig { path: String, tags: Array String }

determineDocRoot :: Effect String
determineDocRoot =


processFile :: FileProcessConfig -> Effect
processFile (FileProcessConfig config) = do
  

main = do
  let setup = usage "$0 [--tag x [--tag y]] <file> [<file2>...]"
  runY setup $ app 
    <$> rest
    <*> yarg "t" ["tag"] Nothing (Left []) false
