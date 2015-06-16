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

generate :: Config -> String -> Api m -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO ()
generate config name api sources imports rewrites = do
  res <- runGenerate config name api sources imports rewrites
  case res of
    Error _err -> exitFailure
    _          -> exitSuccess

runGenerate :: Config -> String -> Api m -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO Result
runGenerate config name api sources imports rewrites =
  withVersion (get apiVersion config) api (return $ Error CouldNotFindApiVersion) m
  where
    m :: Version -> Some1 (Router m) -> IO Result
    m ver (Some1 r) = case get action config of
      Just (MakeDocs root) -> generateDocs       config ver r root
      Just MakeJS          -> generateJavaScript config ver r moduleName
      Just MakeRb          -> generateRuby       config ver r moduleName
      Just MakeHS          -> generateHaskell    config ver r moduleName packageName sources imports rewrites
      Nothing              -> return $ Error NoOp
    packageName = map toLower name
    moduleName  = ModuleName $ upFirst packageName

generateJavaScript :: Config -> Version -> Router m s -> ModuleName -> IO Result
generateJavaScript config ver r moduleName = do
  file <- mkJsApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r
  toTarget config file

generateRuby ::  Config -> Version -> Router m s -> ModuleName -> IO Result
generateRuby config ver r moduleName = do
  file <- mkRbApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r
  toTarget config file

generateDocs :: Config -> Version -> Router m s -> String -> IO Result
generateDocs config ver r rootUrl = do
  targetDir <- getTargetDir config "./docs"
  writeDocs (context targetDir) r
  return $ FileOut targetDir
    where
      context targetDir = DocsContext
        { DCtx.rootUrl        = rootUrl
        , DCtx.contextVersion = ver
        , DCtx.templates      = "./templates" `fromMaybe` getSourceLocation config
        , DCtx.targetDir      = targetDir
        , DCtx.sourceDir      = getSourceLocation config
        }

generateHaskell :: Config -> Version -> Router m s -> ModuleName -> String -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO Result
generateHaskell config ver r moduleName packageName sources imports rewrites = do
  targetPath <- getTargetDir config "./client"
  mkHsApi (context targetPath (getSourceLocation config)) r
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
