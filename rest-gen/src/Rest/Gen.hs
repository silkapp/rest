module Rest.Gen
  ( generate
  , runGenerate
  , generateDocs
  , generateHaskell
  , generateJavaScript
  , generateRuby
  , GenerateError (..)
  , Result (..)
  ) where

import Data.Char
import Data.Label
import Data.Maybe
import System.Exit
import System.IO (hPutStrLn, stderr)

import Rest.Api (Api, Router, Some1 (..), Version, withVersion)

import Rest.Gen.Config
import Rest.Gen.Docs (DocsContext (DocsContext), writeDocs)
import Rest.Gen.Haskell (HaskellContext (HaskellContext), mkHsApi)
import Rest.Gen.JavaScript (mkJsApi)
import Rest.Gen.Ruby (mkRbApi)
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.Docs    as DCtx (DocsContext (..))
import qualified Rest.Gen.Haskell as HCtx (HaskellContext (..))

data GenerateError
  = CouldNotFindApiVersion
  | NoOp
  deriving (Eq, Show)

data Result
  = Error GenerateError
  | StdOut String
  | FileOut FilePath
  deriving (Eq, Show)

data FileType
  = HaskellFile
  | JavaScriptFile
  | RubyFile
  | HtmlFile

generate :: Config -> String -> Api m -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> (FileType -> String -> IO String) -> IO ()
generate config name api sources imports rewrites postProc = do
  res <- runGenerate config name api sources imports rewrites postProc
  case res of
    Error err -> do
      case err of
        CouldNotFindApiVersion -> hPutStrLn stderr "Could not find specified API version"
        NoOp                   -> hPutStrLn stderr "Nothing to do"
      exitFailure
    _          -> exitSuccess

runGenerate :: Config -> String -> Api m -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> (FileType -> String -> IO String) -> IO Result
runGenerate config name api sources imports rewrites postProc =
  withVersion (get apiVersion config) api (return $ Error CouldNotFindApiVersion) m
  where
    m :: Version -> Some1 (Router m) -> IO Result
    m ver (Some1 r) = case get action config of
      Just (MakeDocs root) -> generateDocs       config ver r (postProc HtmlFile      ) root
      Just MakeJS          -> generateJavaScript config ver r (postProc JavaScriptFile) moduleName
      Just MakeRb          -> generateRuby       config ver r (postProc RubyFile      ) moduleName
      Just MakeHS          -> generateHaskell    config ver r (postProc HaskellFile   ) moduleName packageName sources imports rewrites
      Nothing              -> return $ Error NoOp
    packageName = map toLower name
    moduleName  = ModuleName $ upFirst packageName

generateJavaScript :: Config -> Version -> Router m s -> (String -> IO String) -> ModuleName -> IO Result
generateJavaScript config ver r postProc moduleName = do
  file <- postProc =<< mkJsApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r
  toTarget config file

generateRuby ::  Config -> Version -> Router m s -> (String -> IO String) -> ModuleName -> IO Result
generateRuby config ver r postProc moduleName = do
  file <- postProc =<< mkRbApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r
  toTarget config file

generateDocs :: Config -> Version -> Router m s -> (String -> IO String) -> String -> IO Result
generateDocs config ver r postProc rootUrl = do
  targetDir <- getTargetDir config "./docs"
  writeDocs (getSourceLocation config) targetDir context postProc r
  return $ FileOut targetDir
    where
      context = DocsContext
        { DCtx.rootUrl        = rootUrl
        , DCtx.contextVersion = ver
        , DCtx.templates      = "./templates" `fromMaybe` getSourceLocation config
        }

generateHaskell :: Config -> Version -> Router m s -> (String -> IO String) -> ModuleName -> String -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO Result
generateHaskell config ver r postProc moduleName packageName sources imports rewrites = do
  targetPath <- getTargetDir config "./client"
  mkHsApi (context targetPath (getSourceLocation config)) postProc r
  return $ FileOut targetPath
  where
    context tp sourceDir = HaskellContext
      { HCtx.apiVersion     = ver
      , HCtx.targetPath     = tp
      , HCtx.wrapperName    = packageName ++ "-client"
      , HCtx.includePrivate = get apiPrivate config
      , HCtx.sources        = sources
      , HCtx.imports        = imports
      , HCtx.rewrites       = rewrites
      , HCtx.namespace      = [unModuleName moduleName, "Client"]
      , HCtx.sourceDir      = sourceDir
      }

getTargetDir :: Config -> String -> IO String
getTargetDir config str =
  case get target config of
    Stream     -> putStrLn ("Cannot generate file tree to stdOut, generating to " ++ str) >> return str
    Default    -> putStrLn ("Generating to " ++ str)                                      >> return str
    Location d -> putStrLn ("Generating to " ++ d)                                        >> return d

toTarget :: Config -> String -> IO Result
toTarget config code = do
  outf code
  where
    outf cd = case get target config of
      Stream     -> putStrLn cd    >> return (StdOut cd)
      Default    -> putStrLn cd    >> return (StdOut cd)
      Location l -> writeFile l cd >> return (FileOut l)

getSourceLocation :: Config -> Maybe String
getSourceLocation config =
  case get source config of
    Location s -> Just s
    _          -> Nothing
