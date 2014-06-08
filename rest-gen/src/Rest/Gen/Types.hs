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
  ) where

-- import Code.Build
import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.SrcLoc (noLoc)

unModuleName :: H.ModuleName -> String
unModuleName (H.ModuleName name) = name

overModuleName :: (String -> String) -> H.ModuleName -> H.ModuleName
overModuleName f = H.ModuleName . f . unModuleName

-- | Create a simple named basic import, to be updated with other fields
--   as needed.
namedImport :: String -> H.ImportDecl
namedImport name = H.ImportDecl { H.importLoc       = noLoc,
                                  H.importQualified = False,
                                  H.importModule    = H.ModuleName name,
                                  H.importSrc       = False,
                                  H.importPkg       = Nothing,
                                  H.importAs        = Nothing,
                                  H.importSpecs     = Nothing }

-- | Qualified import with given name
qualImport :: String -> H.ImportDecl
qualImport name = (namedImport name) { H.importQualified = True }

haskellStringType :: H.Type
haskellStringType = haskellSimpleType "String"

haskellByteStringType :: H.Type
haskellByteStringType = haskellSimpleType "ByteString"

haskellSimpleType :: String -> H.Type
haskellSimpleType = H.TyCon . H.UnQual . H.Ident

haskellUnitType :: H.Type
haskellUnitType = H.TyTuple H.Boxed []
