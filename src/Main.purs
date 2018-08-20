module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Reader (Reader)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup)
import Data.String.Common (joinWith, trim)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Node.Process (lookupEnv)
import Node.Yargs.Applicative (rest, runY, yarg)
import Node.Yargs.Setup (usage)
import Paths (PathFormat, formatPath, nodePath, parsePath)
import Runner (ensureRun, runAndCapture)

type StoreConfig = 
  { storeBase :: String
  , formatPath :: PathFormat -> String
  , parsePath :: String -> PathFormat
  , filePath :: String
  , sha1 :: String
  , tags :: Array String
  }

data ProcessFile a = ProcessFile (StoreConfig -> Aff a)

instance processFileSemigroup :: (Semigroup a) => Semigroup (ProcessFile a) where
  append (ProcessFile a) (ProcessFile b) = ProcessFile $ a <> b

instance processFileMonoid :: (Monoid a) => Monoid (ProcessFile a) where
  mempty = ProcessFile mempty

instance processFileFunctor :: Functor ProcessFile where
  map f (ProcessFile a) = ProcessFile $ \c -> f <$> a c

instance processFileApply :: Apply ProcessFile where
  apply (ProcessFile f) (ProcessFile a) = ProcessFile $ \c -> f c <*> a c

instance processFileApplicative :: Applicative ProcessFile where
  pure v = ProcessFile $ const $ pure v

instance storeBind :: Bind ProcessFile where
  bind (ProcessFile a) f = ProcessFile $ \c -> a c >>= (\r -> case f r of 
                                       ProcessFile b -> b c)

instance processFileMonad :: Monad ProcessFile

runFileProcess :: forall a. ProcessFile a -> StoreConfig -> Aff a
runFileProcess p config = 
  case prepareDirs *> p of ProcessFile p' -> p' config

originalsDir :: StoreConfig -> String
originalsDir c = c.storeBase <> "/originals"

tagsDir :: StoreConfig -> String
tagsDir c = c.storeBase <> "/tags"

ocrDir :: StoreConfig -> String
ocrDir c = c.storeBase <> "/ocr"

allDirs :: StoreConfig -> Array String
allDirs c = map (_ $ c) [originalsDir, tagsDir, ocrDir]

originalOutputPath :: StoreConfig -> Tuple String String
originalOutputPath c = Tuple (originalsDir c) (c.sha1 <> originalExt)
  where
    originalExt = (c.parsePath c.filePath).ext

tagsOutputPath :: StoreConfig -> Tuple String String
tagsOutputPath c = Tuple (tagsDir c) (c.sha1 <> ".txt")

ocrOutputPath :: StoreConfig -> Tuple String String
ocrOutputPath c = Tuple (ocrDir c) (c.sha1 <> ".txt")

getConfig :: ProcessFile StoreConfig
getConfig = ProcessFile $ pure 

liftAff :: forall a. Aff a -> ProcessFile a
liftAff = ProcessFile <<< const

prepareDirs :: ProcessFile Unit
prepareDirs = do
  config <- getConfig
  let dirs = allDirs config
  liftAff $ traverse_ (\d -> ensureRun "mkdir -p \"$1\"" [d]) dirs

findDropboxHome :: Effect String
findDropboxHome = do
  winhome <- lookupEnv "WINHOME"
  home <- lookupEnv "HOME"
  case winhome <|> home of
    Just trueHome -> pure $ trueHome <> "/Dropbox"
    Nothing -> throwException $ error "Could not determine Dropbox root"

prepareStoreConfig :: Array String -> String -> Aff StoreConfig
prepareStoreConfig tags filePath = do
  dropboxHome <- liftEffect $ findDropboxHome
  pathLib <- liftEffect $ nodePath
  uncleanSha <- runAndCapture "cat \"$1\" | sha1sum | awk '{ print $1 }'" [filePath] 
  let sha1 = trim uncleanSha
  pure $ 
    { storeBase: dropboxHome <> "/store"
    , filePath: filePath
    , tags: tags
    , sha1: sha1
    , formatPath: formatPath pathLib
    , parsePath: parsePath pathLib
    }

tmpFile :: StoreConfig -> String -> String
tmpFile config ext = "/tmp/" <> config.sha1 <> ext

processOcr :: ProcessFile String
processOcr = do
  config <- getConfig
  liftAff do
    let jpegFile = tmpFile config ".png"
    let ocrFile = tmpFile config ".ocr"
    ensureRun "convert \"$1\" \"$2\"" [config.filePath, jpegFile]
    ensureRun "tesseract \"$1\" \"$2\"" [jpegFile, ocrFile]
    pure ocrFile

processTags :: ProcessFile String
processTags = do
  config <- getConfig
  liftAff do
    let tagsFile = tmpFile config ".tags"
    ensureRun "echo \"$1\" \"$2\"" [joinWith "\n" config.tags, tagsFile]
    pure tagsFile

storeFile :: String -> Tuple String String -> ProcessFile Unit
storeFile tmp dest = do
  liftAff do
    ensureRun "mv \"$1\" \"$2\"" [tmp, (fst dest) <> "/" <> (snd dest)]

processNewDoc :: ProcessFile String
processNewDoc = do
  config <- getConfig
  let originalDest = originalOutputPath config
  let tagsDest = tagsOutputPath config
  let ocrDest = ocrOutputPath config
  ocrResultPath <- processOcr
  tagsResultPath <- processTags
  storeFile ocrResultPath ocrDest
  storeFile tagsResultPath tagsDest
  storeFile config.filePath originalDest
  pure $ snd originalDest


app :: Array String -> Array String -> Effect Unit
app [] _ = pure unit
app files tags = do
  dropboxHome <- findDropboxHome
  launchAff_ do
    traverse_ (prepareStoreConfig tags >=> runFileProcess processNewDoc >=> liftEffect <<< log) files

main = do
  let setup = usage "$0 [--tag x [--tag y]] <file> [<file2>...]"
  runY setup $ app
    <$> rest
    <*> yarg "t" ["tag"] Nothing (Left []) false
