module Main (main) where

import Prelude.Compat

import Rest.Gen.Types
import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen

import qualified Api

main :: IO ()
main = do
  config <- Gen.configFromArgs "rest-example-gen"
  Gen.generate
    config
    "RestExample"
    Api.api
    [] -- Additional modules to put in the generated cabal file
    [] -- Additional imports in every module, typically used for orphan instances
    -- rest-gen finds the originating module for each data type, when
    -- these are re-exported from an internal module they can be
    -- rewritten to something more stable.
    [(ModuleName () "Data.Text.Internal", ModuleName () "Data.Text")]
