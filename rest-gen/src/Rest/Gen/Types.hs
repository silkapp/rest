module Rest.Gen.Types
  ( ModuleName (..)
  , overModuleName
  , Import (..)
  , Qualification (..)
  , QName (..)
  , Name (..)
  ) where

import Data.List

import Code.Build

newtype ModuleName = ModuleName { unModuleName :: String }
  deriving (Eq, Show)

instance Codeable ModuleName where
  code = code . unModuleName

overModuleName :: (String -> String) -> ModuleName -> ModuleName
overModuleName f = ModuleName . f . unModuleName

newtype Name = Name { unName :: String }

instance Codeable Name where
  code = code . unName

data QName
  = Qual ModuleName Name
  | UnQual Name

instance Codeable QName where
  code (UnQual n) = code n
  code (Qual m n) = code m <+> "." <+> code n

data Qualification = Qualified | UnQualified

instance Codeable Qualification where
  code Qualified   = code "qualified"
  code UnQualified = code ""

data Import
  = Import Qualification ModuleName (Maybe ModuleName) (Maybe [QName])

instance Codeable Import where
  code i = case i of
    Import q m mas ids
      -> "import"
      <++> q
      <++> m
      <++> qualAs
      <++> maybe (code "") (\v -> "(" <+> impList v <+> ")") ids
      where
        qualAs = case mas of
          Just as
            | as == m   -> code ""
            | otherwise -> "as" <++> as
          Nothing       -> code ""
        impList :: [QName] -> Code
        impList = foldl' (<++>) (code "") . intersperse (code ",") . map code
