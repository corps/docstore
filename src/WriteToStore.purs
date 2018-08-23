module WriteToStore where

import Paths
import Prelude
import Runner
import ScriptBuilder
import Store

import Control.Monad.Error.Class (catchError, throwError)
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe, fromMaybe, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.String (trim)
import Data.Traversable (traverse)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Exception (error)

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

  traverse (install build args.p) tagPaths
  where tagPaths = args.tagPaths <> [ args.storeConfig.allRoot ]

prepareBuildSource :: WriteToStoreArgs -> Aff DocSource
prepareBuildSource args = do
  sha <- getShaOfPath args.originalFullPath
  let buildDir = append args.storeConfig.tmpDir $ segment $ unwrap sha

  mkDir buildDir
  prepareBuildSource' args sha buildDir `catchError` (\e -> do 
    rmDir buildDir
    throwError e)

prepareBuildSource' :: WriteToStoreArgs -> Sha -> Path -> Aff DocSource
prepareBuildSource' args sha buildDir = do
  fmt <- getFormat args.originalFullPath
  let name = fromMaybe fmt.name args.newName
  let ext = Ext fmt.ext
  let namePath = (segment name) `addExt` ext

  runScript ensureRun do
    orig' <- asArg $ show $ args.originalFullPath
    new' <- asArg $ show $ buildDir <> namePath
    addWords ["cp", orig', new']

  runScript ensureRun do
    origPath' <- asArg $ show $ docOriginalPath buildDir
    name' <- asArg $ show namePath
    addWords ["echo", name', ">", origPath']

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
      original' <- asArg $ show original
      tiff' <- asArg $ show tiff
      addWords [ "convert", "-density", "300", original', "-depth", "8", tiff' ]

  runScript ensureRun do
    tiff' <- asArg $ show tiff
    ocr' <- asArg $ show ocr
    addWord "tesseract"
    addWords $ realRelativePath tiff'
    addWords $ realRelativePath ocr'

  runScript ensureRun do
    tiff' <- asArg $ show tiff
    addWords [ "rm", tiff' ]

install :: DocSource -> Boolean -> Path -> Aff DocSource
install docSrc create destRoot = do
  let src = docSrc.parentDir
  let dest = append destRoot $ segment $ unwrap docSrc.sha
  when create $ mkDir destRoot
  runScript ensureRun do
    src' <- asArg $ show src
    dest' <- asArg $ show destRoot
    addWords ["cp", "-r", src', dest']
  pure $ docSrc { parentDir = dest }

getShaOfPath :: Path -> Aff Sha
getShaOfPath path = do
  uncleanSha <- runScript runAndCapture do
    path' <- asArg $ show path
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

