module Runner
  ( RunOpts
  , defaultRunOpts
  , RunResult
  , run
  , runAndCapture
  , ensureRun
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.Posix (Gid, Uid)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (error)
import Foreign.Object (Object)

type RunOpts =
  { quiet :: Boolean
  , capture :: Boolean
  , env :: Maybe (Object String)
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }

defaultRunOpts :: RunOpts
defaultRunOpts =
  { quiet: false
  , capture: true
  , env: Nothing
  , uid: Nothing
  , gid: Nothing
  }

foreign import undefined :: forall a. a
defaultUndefined :: forall a. Maybe a -> a
defaultUndefined = fromMaybe undefined

foreign import runImpl
  :: forall r. String
  -> Array String
  -> { | r }
  -> EffectFnAff { code :: Int, captured :: String }

type RunResult = Either Int String

run
  :: String
  -> Array String
  -> RunOpts
  -> Aff RunResult
run cmd args opts = do
  result <- go
  pure $ if result.code == 0
    then Right result.captured
    else Left result.code
  where
    go = fromEffectFnAff $ runImpl cmd args opts'
    opts' =
      { quiet: opts.quiet
      , capture: opts.capture
      , env: toNullable opts.env
      , uid: defaultUndefined opts.uid
      , gid: defaultUndefined opts.gid
      }

runAndCapture :: String -> Array String -> Aff String
runAndCapture cmd args = do
  result <- run cmd args defaultRunOpts
  case result of
    Left code -> throwError $ error $ "Command failed: " <> cmd <> " with status " <> (show code)
    Right output -> pure output

ensureRun :: String -> Array String -> Aff Unit
ensureRun cmd args = do
  result <- run cmd args $ defaultRunOpts { capture = false }
  case result of
    Left code -> throwError $ error $ "Command failed: " <> cmd <> " with status " <> (show code)
    Right output -> pure unit

