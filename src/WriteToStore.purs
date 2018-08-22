module WriteToStore where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store

import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe, fromMaybe, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.String (trim)
import Data.Traversable (traverse)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)

type WriteToStoreArgs =
  { originalFullPath :: Path
  , storeConfig :: StoreConfig
  , tagPaths :: Array Path
  , p :: Boolean
  , newName :: Maybe String
  }

type WriteToStoreRes = Array DocSource

writeToStore :: WriteToStoreArgs -> Aff WriteToStoreRes
writeToStore args = bracket (prepareBuildSource args) 
  cleanupBuildSource 
  (writeToStore' args)
  
writeToStore' :: WriteToStoreArgs -> DocSource -> Aff WriteToStoreRes
writeToStore' args build = do
  case unwrap build.originalExt of
    ".pdf"  -> processOcr build
    ".tiff" -> processOcr build
    otherwise -> pure unit

  traverse (install build args.p) args.tagPaths

prepareBuildSource :: WriteToStoreArgs -> Aff DocSource
prepareBuildSource args = do
  sha <- getShaOfPath args.originalFullPath
  fmt <- getFormat args.originalFullPath

  let name = fromMaybe fmt.name args.newName
  let buildDir = append args.storeConfig.tmpDir $ segment $ unwrap sha
  let ext = Ext fmt.ext
  mkDir buildDir
  
  runScript ensureRun do
    orig' <- asArg args.originalFullPath
    new' <- asArg $ buildDir <> (segment name) `addExt` ext
    addWords ["cp", orig', new']

  pure $ { sha: sha
         , originalExt: ext
         , originalName: segment name
         , parentDir: buildDir
         }

cleanupBuildSource :: DocSource -> Aff Unit
cleanupBuildSource docSource = rmDir docSource.parentDir

processOcr :: DocSource -> Aff Unit
processOcr docSrc = do
  let original = docOriginal docSrc
  let tiff = docFile docSrc $ Ext ".tiff"
  let ocr = docFile docSrc $ Ext ".ocr"

  when (docSrc.originalExt /= Ext ".tiff") do
    runScript ensureRun do
      original' <- asArg original
      tiff' <- asArg tiff
      addWords [ "convert", "-density", "300", original', "-depth", "8", tiff' ]

  runScript ensureRun do
    tiff' <- asArg tiff
    ocr' <- asArg ocr
    addWord "tesseract"
    addWords $ realRelativePath tiff'
    addWords $ realRelativePath ocr'

install :: DocSource -> Boolean -> Path -> Aff DocSource
install docSrc create destRoot = do
  let src = docSrc.parentDir
  let dest = append destRoot $ segment $ unwrap docSrc.sha
  when create $ mkDir destRoot
  runScript ensureRun do
    src' <- asArg src
    dest' <- asArg dest
    addWords ["cp", "-r", src', dest']
  pure $ docSrc { parentDir = dest }

getShaOfPath :: Path -> Aff Sha
getShaOfPath path = do
  uncleanSha <- runScript runAndCapture do
    path' <- asArg path
    addWords [ "cat" , path' ]
    addPipe
    addWords [ "sha1sum" ]
    addPipe
    addWords [ "awk", "'{ print $1 }'" ]
  pure $ wrap $ trim uncleanSha

getFormat :: Path -> Aff PathFormat
getFormat p = do
  pathLib <- liftEffect $ nodePath
  pure $ parsePath pathLib $ show p

realRelativePath :: String -> Array String
realRelativePath p = inCapture 
  ["realpath", "--relative-to=" <> inQuotes "$PWD", p]

mkDir :: Path -> Aff Unit
mkDir p = do
  runScript ensureRun do
    p' <- asArg p
    addWords ["mkdir", "-p", p']

rmDir :: Path -> Aff Unit
rmDir p = do
  runScript ensureRun do
    p' <- asArg p
    addWords ["rm", "-rf", p']

