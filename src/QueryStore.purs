module QueryStore (
  QueryMatch,
  QueryStoreArgs,
  QueryStoreRes,
  queryStore,
  showQueryMatch
) where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store

import Control.Monad.Error.Class (throwError)
import Data.Array (filter, init, last, tail, uncons, unsnoc)
import Data.Foldable (foldl)
import Data.Function (const)
import Data.Map (keys)
import Data.Map.Internal (Map, insert, lookup, unionWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (intersection, union)
import Data.String (Pattern(..), trim)
import Data.String.CodeUnits (indexOf, splitAt, take, drop)
import Data.String.Common (split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Unsafe.Coerce (unsafeCoerce)

type QueryStoreArgs =
  { storeConfig :: StoreConfig
  , tagPaths :: Array Path
  , words :: Array String
  }

type QueryMatch =
  { docSource :: DocSource
  , matches :: Array String
  }

type QueryStoreRes = Map String QueryMatch

addQueryMatch :: Aff QueryStoreRes -> Tuple String String -> Aff QueryStoreRes
addQueryMatch affRes (Tuple pathString resultString) = do
  res <- affRes
  let path = segment pathString
  parentDir <- case init $ unwrap path of
    Just p -> pure p
    Nothing -> throwError $ error $ "could not determine root path of query match " <> pathString

  sha <- case last parentDir of
    Just shaString -> pure shaString
    Nothing -> throwError $ error $ "could not determine sha of query match " <> pathString

  let insertMatch :: QueryMatch -> QueryStoreRes
      insertMatch m = insert sha m res

  case lookup sha res of
    Just m -> pure $ insertMatch $ m { matches = m.matches <> [resultString] }
    otherwise -> do
       match <- map ({ matches: [resultString], docSource: _}) $ readDocSource $ Path parentDir
       pure $ insertMatch match

splitGrepLine :: String -> Tuple String String
splitGrepLine line = Tuple (take idx line) (drop (idx + 1) line)
  where idx = fromMaybe 0 $ indexOf (Pattern ":") line

queryTagWord :: Path -> String -> Aff QueryStoreRes
queryTagWord tagPath word = do
  grepOut <- runScript runAndCapture do
    tagPath' <- asArg $ show tagPath
    word' <- asArg word
    addWords ["grep", "--exclude", "orig.path", "-I", "-m", "3", "-i", "-r", word', tagPath', "||", "true"]
  let grepLines = split (Pattern "\n") $ trim grepOut
  let grepLines' = filter (_ /= "") grepLines
  let splitGrepLines = splitGrepLine <$> grepLines'

  foldl addQueryMatch mempty splitGrepLines

queryTag :: Array String -> Path -> Aff QueryStoreRes
queryTag words tagPath = do
  tagsResults <- traverse (queryTagWord tagPath) words
  case uncons tagsResults of
    Just { head, tail } -> pure $ foldl intersectResults head tail
    otherwise -> pure $ mempty

intersectResults :: QueryStoreRes -> QueryStoreRes -> QueryStoreRes
intersectResults a b = foldl reassemble mempty intersectingKeys
  where
    intersectingKeys = keys a `intersection` keys b
    reassemble :: QueryStoreRes -> String -> QueryStoreRes
    reassemble res key =
      let aV = lookup key a
          bV = lookup key b 
          joined = (\x y z -> { docSource: x, matches: y <> z })
            <$> (_.docSource <$> aV)
            <*> (_.matches <$> aV)
            <*> (_.matches <$> bV)
          inserted = insert key <$> joined <*> Just res
      in
      fromMaybe (unsafeCoerce "unreachable") inserted

joinResults :: QueryStoreRes -> QueryStoreRes -> QueryStoreRes
joinResults = unionWith const

queryStore :: QueryStoreArgs -> Aff QueryStoreRes
queryStore args = do
  tagResults <- traverse (queryTag args.words) args.tagPaths
  pure $ foldl joinResults mempty tagResults

showQueryMatch :: QueryMatch -> Array String
showQueryMatch { docSource, matches } =
  [ show $ docSource.parentDir <> docSource.originalName `addExt` docSource.originalExt ]
  <> (map (\x -> "  " <> x) matches)

     
