module Paths where

import Prelude

import Effect (Effect)

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
