module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Applicative (class Applicative, when)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Reader (Reader)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
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

newtype StoreBase = StoreBase String
derive instance storeBaseNewtype :: Newtype StoreBase _

storePath :: StoreBase -> String -> String
storePath (StoreBase a) b = a <> b

type StoreConfig = 
  { storeBase :: StoreBase
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

originalsDir :: StoreBase -> String
originalsDir base = base `storePath` "/originals"

tagsDir :: StoreBase -> String
tagsDir base = base `storePath` "/tags"

ocrDir :: StoreBase -> String
ocrDir base = base `storePath` "/ocr"

allDirs :: StoreConfig -> Array String
allDirs c = map (_ $ c.storeBase) [originalsDir, tagsDir, ocrDir]

originalOutputPath :: StoreConfig -> Tuple String String
originalOutputPath c = Tuple (originalsDir c.storeBase) (c.sha1 <> originalExt)
  where
    originalExt = (c.parsePath c.filePath).ext

tagsOutputPath :: StoreConfig -> Tuple String String
tagsOutputPath c = Tuple (tagsDir c.storeBase) (c.sha1 <> ".txt")

ocrOutputPath :: StoreConfig -> Tuple String String
ocrOutputPath c = Tuple (ocrDir c.storeBase) (c.sha1 <> ".txt")

getConfig :: ProcessFile StoreConfig
getConfig = ProcessFile $ pure 

liftAff :: forall a. Aff a -> ProcessFile a
liftAff = ProcessFile <<< const

prepareDirs :: ProcessFile Unit
prepareDirs = do
  config <- getConfig
  let dirs = allDirs config
  liftAff $ traverse_ (\d -> ensureRun "mkdir -p \"$1\"" [d]) dirs

findStoreBase :: Effect StoreBase
findStoreBase = do
  winhome <- lookupEnv "WINHOME"
  home <- lookupEnv "HOME"
  case winhome <|> home of
    Just trueHome -> pure $ StoreBase $ trueHome <> "/Dropbox/store"
    Nothing -> throwException $ error "Could not determine Dropbox root"

prepareStoreConfig :: Array String -> String -> Aff StoreConfig
prepareStoreConfig tags filePath = do
  storeBase <- liftEffect $ findStoreBase
  pathLib <- liftEffect $ nodePath
  uncleanSha <- runAndCapture "cat \"$1\" | sha1sum | awk '{ print $1 }'" [filePath] 
  let sha1 = trim uncleanSha
  pure $ 
    { storeBase: storeBase
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
    let tiffFile = tmpFile config ".tiff"
    let ocrFile = tmpFile config ".ocr"
    ensureRun "convert -density 300 \"$1\" -depth 8 \"$2\"" [config.filePath, tiffFile]
    ensureRun ("tesseract $(realpath --relative-to=\"$PWD\" \"$1\") " <>
              "$(realpath --relative-to=\"$PWD\" \"$2\")") [tiffFile, ocrFile]
    pure $ ocrFile <> ".txt"

processTags :: ProcessFile String
processTags = do
  config <- getConfig
  liftAff do
    let tagsFile = tmpFile config ".tags"
    ensureRun "echo \"$1\" > \"$2\"" [joinWith "\n" config.tags, tagsFile]
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

openDoc :: String -> Aff Unit
openDoc pattern = do
  storeBase <- liftEffect findStoreBase
  ensureRun "open \"$1\"/$2" [originalsDir storeBase, pattern <> "*"]

logScript :: String
logScript = joinWith " | " script
   where
     script :: Array String
     script = [ "ls -ltc \"$1\"", 
                "tail -n +2", 
                "head -n 20", 
                "awk -vpath=\"$1\"/ '{ print path$9 }'", 
                "xargs -L 1 bash -c 'for path; do stat -c \"%y %n\" $path; echo -n \"  \"; head -n 1 $path; done' bash"
               ]

app :: String -> Array String -> Array String -> Effect Unit
app "store" _ [] = pure unit

app "store" files tags = do
  launchAff_ do
    traverse_ (prepareStoreConfig tags >=> runFileProcess processNewDoc >=> liftEffect <<< log) files

app "log" _ _ = launchAff_ do
  storeBase <- liftEffect findStoreBase
  ensureRun logScript [tagsDir storeBase]

app "query" text tags = do
  launchAff_ do
    storeBase <- liftEffect findStoreBase
    let fullText = joinWith "\n" text
    when (fullText /= "") do 
      liftEffect $ log "By Text:"
      ensureRun "grep -Ri -F \"$1\" \"$2\"" [fullText, ocrDir storeBase]

    let fullTags = joinWith "\n" tags
    when (fullTags /= "") do 
      liftEffect $ log "By Tags:"
      ensureRun "grep -Ri -F \"$1\" \"$2\"" [fullTags, tagsDir storeBase]

app "open" files _ = do
  launchAff_ do
    traverse_ openDoc files

app _ _ _ = pure unit

main = do
  let setup = usage "$0 [--action store|log|query|open] [--tag x --tag y ...] [<file> <file2> ...]"
  runY setup $ app
    <$> yarg "a" ["action"] (Just "the action to take, default: store") (Left "store") false
    <*> rest
    <*> yarg "t" ["tag"] (Just "tags to associate") (Left []) false
