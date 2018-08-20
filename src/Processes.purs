module Processes where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Posix (Gid(..), Pid(..), Uid(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff(..), fromEffectFnAff)
import Effect.Exception as Exception
import Foreign.Object (Object)
import Node.Stream (Writable, Readable)
import Unsafe.Coerce (unsafeCoerce)

-- | Configuration of `spawn`. Fields set to `Nothing` will use
-- | the node defaults.
type SpawnOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , shell :: Maybe String
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }

-- | A default set of `SpawnOptions`. Everything is set to `Nothing`,
-- | `detached` is `false` and `stdio` is `ChildProcess.pipe`.
defaultSpawnOptions :: SpawnOptions
defaultSpawnOptions =
  { cwd: Nothing
  , shell: Nothing
  , env: Nothing
  , uid: Nothing
  , gid: Nothing
  }

foreign import undefined :: forall a. a
defaultUndefined :: forall a. Maybe a -> a
defaultUndefined = fromMaybe undefined

foreign import spawnImpl
  :: forall r. String
  -> Array String
  -> { | r }
  -> EffectFnAff SpawnResult

type SpawnResult = { code :: Int, stdout :: String, stderr :: String }

spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Aff SpawnResult
spawn cmd args opts = fromEffectFnAff $ spawnImpl cmd args opts'
  where
    opts' =
      { cwd: defaultUndefined opts.cwd
      , env: toNullable opts.env
      , shell: defaultUndefined opts.shell
      , uid: defaultUndefined opts.uid
      , gid: defaultUndefined opts.gid
      }
