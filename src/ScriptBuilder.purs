module ScriptBuilder where

import Prelude

import Control.Apply (lift2)
import Control.Monad.RWS (get, put, modify_)
import Control.Monad.State (State, execState)
import Control.Monad.State.Trans (runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, init, last, length, tail, uncons, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Semigroup (class Semigroup)
import Data.String.Common (joinWith)
import Pipes.Prelude (concat)

type ScriptBuilderState =
  { args :: Array String
  , parts :: Array String
  }

emptyScript :: ScriptBuilderState
emptyScript = { args: [], parts: [] }

type ScriptRunner a = String -> Array String -> a

type ScriptBuilder a = State ScriptBuilderState a

runScript :: forall a. ScriptRunner a -> ScriptBuilder Unit -> a
runScript runner s = runner cmd args
  where resultingBuilder = execState s emptyScript
        cmd = joinWith " " resultingBuilder.parts
        args = resultingBuilder.args

inQuotes :: String -> String
inQuotes s = "\"" <> s <> "\""

inCapture :: Array String -> Array String
inCapture parts = fromMaybe [] do
  a <- uncons parts
  let headed = ["$(" <> a.head] <> a.tail
  b <- unsnoc headed
  pure $ b.init <> [b.last <> ")"]

asArg :: String -> ScriptBuilder String
asArg s = do
  state <- get
  let curArgLength = length state.args
  let argStr = inQuotes $ append "$" $ show $ curArgLength + 1
  put $ state { args = state.args <> [s] }
  pure argStr

addWord :: String -> ScriptBuilder Unit
addWord l = do
  modify_ (\s -> s { parts = s.parts <> [l] })

addWords :: Array String -> ScriptBuilder Unit
addWords ws = do
  modify_ (\s -> s { parts = s.parts <> ws })

pipe :: Array String -> Array String -> Array String
pipe a b = a <> ["|"] <> b

addPipe :: ScriptBuilder Unit
addPipe = addWord "|"
