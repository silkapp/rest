module Rest.Gen.Base.ActionInfo.Ident (Ident (..)) where

import Rest.Gen.Types

data Ident = Ident
  { description    :: String
  , haskellType    :: String
  , haskellModules :: [ModuleName]
  } deriving (Show, Eq)
