{-# LANGUAGE CPP #-}
module Rest.Gen.Types
  ( unModuleName
  , overModuleName
  , namedImport
  , qualImport
  , haskellStringType
  , haskellByteStringType
  , haskellUnitType
  , haskellSimpleType
  , haskellVoidType
  , noLoc
  , ModuleName (..)
  , ImportDecl (..)
  ) where

import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Syntax (ImportDecl (..), ModuleName (..), Name (..), QName (..), SpecialCon (..), Type (..))

import qualified Rest.Gen.NoAnnotation as N

unModuleName :: N.ModuleName -> String
unModuleName (ModuleName _ name) = name

overModuleName :: (String -> String) -> N.ModuleName -> N.ModuleName
overModuleName f = ModuleName () . f . unModuleName

-- | Create a simple named basic import, to be updated with other fields
--   as needed.
namedImport :: String -> N.ImportDecl
namedImport name = ImportDecl
  { importAnn       = ()
  , importQualified = False
  , importModule    = ModuleName () name
  , importSrc       = False
  , importSafe      = False
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Nothing
  }

-- | Qualified import with given name
qualImport :: String -> N.ImportDecl
qualImport name = (namedImport name) { importQualified = True }

haskellStringType :: N.Type
haskellStringType = haskellSimpleType "String"

haskellByteStringType :: N.Type
haskellByteStringType = haskellSimpleType "ByteString"

haskellSimpleType :: String -> N.Type
haskellSimpleType = TyCon () . UnQual () . Ident ()

haskellUnitType :: N.Type
haskellUnitType = TyCon () (Special () (UnitCon ()))

haskellVoidType :: N.Type
haskellVoidType = TyCon () (Qual () (ModuleName () "Rest.Types.Void") (Ident () "Void"))
