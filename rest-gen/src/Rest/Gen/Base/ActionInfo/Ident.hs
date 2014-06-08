module Rest.Gen.Base.ActionInfo.Ident (Ident (..)) where

import qualified Language.Haskell.Exts.Syntax as H

data Ident = Ident
  { description    :: String
  , haskellType    :: H.Type
  , haskellModules :: [H.ModuleName]
  } deriving (Show, Eq)
