module Main (main) where

import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen

import qualified Api

main :: IO ()
main = do
  config <- Gen.configFromArgs "rest-example-gen"
  Gen.generate config "RestExample" Api.api [] [] []
