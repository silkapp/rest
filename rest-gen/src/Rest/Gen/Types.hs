module Rest.Gen.Types where

newtype ModuleName = ModuleName { unModuleName :: String }
  deriving (Eq, Show)
