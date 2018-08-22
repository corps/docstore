module Paths where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Common (joinWith)
import Effect (Effect)

newtype Ext = Ext String
derive instance extNewtype :: Newtype Ext _

newtype Path = Path (Array String)
derive instance pathNewType :: Newtype Path _

instance pathShow :: Show Path where
  show = joinWith "/" <<< unwrap

instance pathSemigroup :: Semigroup Path where
  append a b = wrap $ (unwrap a) <> (unwrap b)

addExt :: Path -> Ext -> Path
addExt p e = wrap $ (unwrap p) <> [unwrap e]

segment :: String -> Path
segment a = Path [a]

root :: Path
root = Path [""]

type PathFormat =
  { dir :: String
  , root :: String
  , base :: String
  , name :: String
  , ext :: String
  }

foreign import data PathLib :: Type
foreign import nodePath :: Effect PathLib

foreign import parsePath :: PathLib -> String -> PathFormat
foreign import formatPath :: PathLib -> PathFormat -> String
