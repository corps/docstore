module Paths where

import Prelude
import Runner
import ScriptBuilder

import Data.Array (last, unsnoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..))
import Data.String.Common (joinWith, split)
import Effect (Effect)
import Effect.Aff (Aff)

newtype Ext = Ext String
derive instance extNewtype :: Newtype Ext _
derive instance extEq :: Eq Ext

newtype Path = Path (Array String)
derive instance pathNewType :: Newtype Path _
derive instance pathEq :: Eq Path

instance pathShow :: Show Path where
  show (Path p) = if p == [""] then "/" else joinWith "/" p

instance pathSemigroup :: Semigroup Path where
  append a b = wrap $ (unwrap a) <> (unwrap b)

addExt :: Path -> Ext -> Path
addExt (Path p) (Ext e) = case (unsnoc p) of
  Just { init, last } -> Path $ init <> [last <> e]
  Nothing -> Path []

foreign import segmentImpl :: String -> Array String

segment :: String -> Path
segment = segmentImpl >>> wrap

root :: Path
root = Path [""]

type PathFormat =
  { dir :: String
  , root :: String
  , base :: String
  , name :: String
  , ext :: String
  }

checkExists :: Path -> Aff Unit
checkExists path =
  runScript ensureRun do
    path' <- asArg $ show path
    addWords ["[[", "-e", path', "]]"]

mkDir :: Path -> Aff Unit
mkDir p = do
  runScript ensureRun do
    p' <- asArg $ show p
    addWords ["mkdir", "-p", p']

rmDir :: Path -> Aff Unit
rmDir p = do
  runScript ensureRun do
    p' <- asArg $ show p
    addWords ["rm", "-rf", p']


foreign import data PathLib :: Type
foreign import nodePath :: Effect PathLib

foreign import parsePath :: PathLib -> String -> PathFormat
foreign import formatPath :: PathLib -> PathFormat -> String
