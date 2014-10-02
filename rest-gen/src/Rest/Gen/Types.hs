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
  , noLoc
  , ModuleName (..)
  , ImportDecl (..)
  ) where

import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Syntax (ImportDecl (..), ModuleName (..), Name (..), QName (..), SpecialCon (..), Type (..))

unModuleName :: ModuleName -> String
unModuleName (ModuleName name) = name

overModuleName :: (String -> String) -> ModuleName -> ModuleName
overModuleName f = ModuleName . f . unModuleName

-- | Create a simple named basic import, to be updated with other fields
--   as needed.
namedImport :: String -> ImportDecl
namedImport name = ImportDecl
  { importLoc       = noLoc
  , importQualified = False
  , importModule    = ModuleName name
  , importSrc       = False
#if MIN_VERSION_haskell_src_exts(1,16,0)
  , importSafe      = False
#endif
  , importPkg       = Nothing
  , importAs        = Nothing
  , importSpecs     = Nothing
  }

-- | Qualified import with given name
qualImport :: String -> ImportDecl
qualImport name = (namedImport name) { importQualified = True }

haskellStringType :: Type
haskellStringType = haskellSimpleType "String"

haskellByteStringType :: Type
haskellByteStringType = haskellSimpleType "ByteString"

haskellSimpleType :: String -> Type
haskellSimpleType = TyCon . UnQual . Ident

haskellUnitType :: Type
haskellUnitType = TyCon (Special UnitCon)
