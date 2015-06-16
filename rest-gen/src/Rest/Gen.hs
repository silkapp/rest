module Rest.Gen (generate) where

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

generate :: Config -> String -> Api m -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO ()
generate config name api sources imports rewrites =
  withVersion (get apiVersion config) api (putStrLn "Could not find api version" >> exitFailure) $ \ver (Some1 r) ->
     case get action config of
       Just (MakeDocs root) -> makeDocs config ver r root >> exitSuccess
       Just MakeJS          -> makeJS config ver r moduleName
       Just MakeRb          -> makeRb config ver r moduleName
       Just MakeHS          -> makeHS config ver r moduleName packageName sources imports rewrites >> exitSuccess
       Nothing              -> return ()
  where
    packageName = map toLower name
    moduleName  = ModuleName $ upFirst packageName

makeDocs :: Config -> Version -> Router m s -> String -> IO ()
makeDocs config ver r rootUrl = do
  targetDir <- getTargetDir config "./docs"
  writeDocs (context targetDir) r
    where
      context targetDir = DocsContext
        { DCtx.rootUrl        = rootUrl
        , DCtx.contextVersion = ver
        , DCtx.templates      = "./templates" `fromMaybe` getSourceLocation config
        , DCtx.targetDir      = targetDir
        , DCtx.sourceDir      = getSourceLocation config
        }

makeJS :: Config -> Version -> Router m s -> ModuleName -> IO ()
makeJS config ver r moduleName = mkJsApi moduleName (get apiPrivate config) ver r >>= toTarget config

makeRb ::  Config -> Version -> Router m s -> ModuleName -> IO ()
makeRb config ver r moduleName = mkRbApi moduleName (get apiPrivate config) ver r >>= toTarget config

makeHS :: Config -> Version -> Router m s -> ModuleName -> String -> [ModuleName] -> [ImportDecl] -> [(ModuleName, ModuleName)] -> IO ()
makeHS config ver r moduleName packageName sources imports rewrites = do
  targetPath <- getTargetDir config "./client"
  mkHsApi (context targetPath (getSourceLocation config)) r
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

toTarget :: Config -> String -> IO ()
toTarget config code = do
   outf code
   exitSuccess
  where
    outf = case get target config of
      Stream     -> putStrLn
      Default    -> putStrLn
      Location l -> writeFile l

getSourceLocation :: Config -> Maybe String
getSourceLocation config =
  case get source config of
    Location s -> Just s
    _          -> Nothing
