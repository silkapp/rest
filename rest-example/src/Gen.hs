{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Label
import System.Console.GetOpt
import qualified Config.Parse    as C
import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen

import Paths_rest_example
import qualified Api

data Config = Config
  { _genConfig  :: Gen.Config
  , _configFile :: String
  }

mkLabels [''Config]

defaultConfig :: Config
defaultConfig = Config Gen.defaultConfig "restexamplegenrc"

options :: [OptDescr (Config -> Config)]
options = Gen.options genConfig

getConfig :: IO Config
getConfig = C.getConfig version $(C.gitVersion) defaultConfig options (get configFile)

main :: IO ()
main = do
  config <- getConfig
  Gen.generate (get genConfig config) "RestExample" Api.api [] [] []
  putStrLn "Done"
