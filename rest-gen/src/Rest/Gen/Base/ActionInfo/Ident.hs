module Rest.Gen.Base.ActionInfo.Ident (Ident (..)) where

import Prelude.Compat

import qualified Rest.Gen.NoAnnotation as N

data Ident = Ident
  { description    :: String
  , haskellType    :: N.Type
  , haskellModules :: [N.ModuleName]
  } deriving (Show, Eq)
