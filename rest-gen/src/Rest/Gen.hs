module Rest.Gen
  ( generate
  ) where

import Data.Char
import Data.Foldable
import Data.Label
import Data.Maybe
import System.Directory
import System.Exit
import System.Process
import qualified Language.Haskell.Exts.Syntax as H

import Rest.Api (Api, Some1 (..), withVersion)

import Rest.Gen.Config
import Rest.Gen.Docs (DocsContext (DocsContext), writeDocs)
import Rest.Gen.Haskell (HaskellContext (HaskellContext), mkHsApi)
import Rest.Gen.JavaScript (mkJsApi)
import Rest.Gen.Ruby (mkRbApi)
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.NoAnnotation as N

generate :: Config -> String -> Api m -> [N.ModuleName] -> [N.ImportDecl] -> [(N.ModuleName, N.ModuleName)] -> IO ()
generate config name api sources imports rewrites =
  withVersion (get apiVersion config) api (putStrLn "Could not find api version" >> exitFailure) $ \ver (Some1 r) ->
     case get action config of
       Just (MakeDocs root) ->
         do loc <- getTargetDir config "./docs"
            setupTargetDir config loc
            let context = DocsContext root ver (fromMaybe "./templates" (getSourceLocation config))
            writeDocs context r loc
            exitSuccess
       Just MakeJS          -> mkJsApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r >>= toTarget config
       Just MakeRb          -> mkRbApi (overModuleName (++ "Api") moduleName) (get apiPrivate config) ver r >>= toTarget config
       Just MakeHS          ->
         do loc <- getTargetDir config "./client"
            setupTargetDir config loc
            let context = HaskellContext ver loc (packageName ++ "-client") (get apiPrivate config) sources imports rewrites [unModuleName moduleName, "Client"]
            mkHsApi context r
            exitSuccess
       Nothing              -> return ()
  where
    packageName = map toLower name
    moduleName  = H.ModuleName () $ upFirst packageName

getTargetDir :: Config -> String -> IO String
getTargetDir config str =
  case get target config of
    Stream     -> putStrLn ("Cannot generate documentation to stdOut, generating to " ++ str) >> return str
    Default    -> putStrLn ("Generating to " ++ str) >> return str
    Location d -> putStrLn ("Generating to " ++ d) >> return d

setupTargetDir :: Config -> String -> IO ()
setupTargetDir config t =
  do createDirectoryIfMissing True t
     forM_ (getSourceLocation config) $ \s -> system $ "cp -rf " ++ s ++ " " ++ t

toTarget :: Config -> String -> IO ()
toTarget config code =
  do let outf =
           case get target config of
             Stream     -> putStrLn
             Default    -> putStrLn
             Location l -> writeFile l
     outf code
     exitSuccess

getSourceLocation :: Config -> Maybe String
getSourceLocation config =
  case get source config of
    Location s -> Just s
    _          -> Nothing
