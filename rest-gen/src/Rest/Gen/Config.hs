{-# LANGUAGE
    TemplateHaskell
  , TypeOperators
  #-}
module Rest.Gen.Config
  ( Action (..)
  , Location (..)

  , Config
  , action
  , source
  , target
  , apiVersion
  , apiPrivate
  , compiler
  , defaultConfig
  , parseLocation
  , options
  , configFromArgs
  ) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Category
import Data.Label
import System.Console.GetOpt
import System.Environment (getArgs)
import Rest.Gen.JSCompiler

data Action   = MakeDocs String | MakeJS | MakeRb | MakeHS
data Location = Default | Stream | Location String

data Config = Config
  { _action     :: Maybe Action
  , _source     :: Location
  , _target     :: Location
  , _apiVersion :: String
  , _apiPrivate :: Bool
  , _compiler :: Maybe Compiler
  }

mkLabels [''Config]

defaultConfig :: Config
defaultConfig = Config
  { _action     = Nothing
  , _source     = Default
  , _target     = Default
  , _apiVersion = "latest"
  , _apiPrivate = True
  , _compiler   = Nothing
  }

parseLocation :: String -> Location
parseLocation "-" = Stream
parseLocation s   = Location s

parseCompiler "closure-simple" = Just ClosureSimple
parseCompiler "closure-advanced" = Just ClosureAdvanced
parseCompiler "whitespace" = Just ClosureWhitespace
parseCompiler _ = Nothing

options :: (a :-> Config) -> [OptDescr (a -> a)]
options parent =
  [ Option ['d'] ["documentation"] (ReqArg (set (action     . parent) . Just . MakeDocs) "URLROOT") "Generate API documentation, available under the provided URL root."
  , Option ['j'] ["javascript"]    (NoArg  (set (action     . parent) (Just MakeJS))) "Generate Javascript bindings."
  , Option ['r'] ["ruby"]          (NoArg  (set (action     . parent) (Just MakeRb))) "Generate Ruby bindings."
  , Option ['h'] ["haskell"]       (NoArg  (set (action     . parent) (Just MakeHS))) "Generate Haskell bindings."
  , Option ['s'] ["source"]        (ReqArg (set (source     . parent) . parseLocation) "LOCATION") "The location of additional sources."
  , Option ['t'] ["target"]        (ReqArg (set (target     . parent) . parseLocation) "LOCATION") "The target location for generation."
  , Option ['v'] ["version"]       (ReqArg (set (apiVersion . parent)) "VERSION") "The version of the API under generation. Default latest."
  , Option ['p'] ["hide-private"]  (NoArg  (set (apiPrivate . parent) False)) "Generate API for the public, hiding private resources. Not default."
  , Option ['c'] ["compiler"]      (ReqArg (set (compiler   . parent) . parseCompiler) "COMPILER") "Choose a compiler for javascript.  \nDefaults to None. \nOptions are closure-simple, closure-advanced, whitespace and command. \nHas no effect if not generating javascript api"
  ]

configFromArgs :: String -> IO Config
configFromArgs name = do
  let header = "Usage: " ++ name ++ " [OPTIONS...], with the following options:"
  args <- getArgs
  fst <$> processArgs defaultConfig (options id) header args

processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defConfig opts header args =
    case getOpt Permute opts args of
        (oargs, nonopts, []    ) -> return (foldl (flip ($)) defConfig oargs, nonopts)
        (_    , _      , errors) -> ioError $ userError $ concat errors ++ usageInfo header opts
