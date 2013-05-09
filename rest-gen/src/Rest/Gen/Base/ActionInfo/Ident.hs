module Rest.Gen.Base.ActionInfo.Ident (Ident (..)) where

data Ident = Ident
  { description   :: String
  , haskellType   :: String
  , haskellModule :: [String]
  } deriving (Show, Eq)
