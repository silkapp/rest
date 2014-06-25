{-# LANGUAGE
    DoAndIfThenElse
  , PatternGuards
  , TemplateHaskell
  #-}
module Rest.Gen.Haskell
  ( HaskellContext (..)
  , mkHsApi
  ) where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Category
import Control.Monad
import Data.Label (modify, set)
import Data.Label.Derive (mkLabelsNamed)
import Data.List
import Data.Maybe
import Prelude hiding (id, (.))
import Safe
import System.Directory
import System.FilePath
import qualified Data.Generics.Uniplate.Data                 as U
import qualified Distribution.ModuleName                     as Cabal
import qualified Distribution.Package                        as Cabal
import qualified Distribution.PackageDescription             as Cabal
import qualified Distribution.PackageDescription.Parse       as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Simple.Utils                   as Cabal
import qualified Distribution.Verbosity                      as Cabal
import qualified Distribution.Version                        as Cabal
import qualified Language.Haskell.Exts.Pretty                as H
import qualified Language.Haskell.Exts.Syntax                as H

import Rest.Api (Router, Version)

import Rest.Gen.Base
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.Base.ActionInfo.Ident as Ident

mkLabelsNamed ("_" ++) [''Cabal.GenericPackageDescription, ''Cabal.CondTree, ''Cabal.Library]

data HaskellContext =
  HaskellContext
    { apiVersion     :: Version
    , targetPath     :: String
    , wrapperName    :: String
    , includePrivate :: Bool
    , sources        :: [H.ModuleName]
    , imports        :: [H.ImportDecl]
    , rewrites       :: [(H.ModuleName, H.ModuleName)]
    , namespace      :: [String]
    }

mkHsApi :: HaskellContext -> Router m s -> IO ()
mkHsApi ctx r =
  do let tree = sortTree . (if includePrivate ctx then id else noPrivate) . apiSubtrees $ r
     mkCabalFile ctx tree
     mapM_ (writeRes ctx) $ allSubTrees tree

mkCabalFile :: HaskellContext -> ApiResource -> IO ()
mkCabalFile ctx tree =
  do cabalExists <- doesFileExist cabalFile
     gpkg <-
       if cabalExists
       then updateExposedModules modules <$> Cabal.readPackageDescription Cabal.normal cabalFile
       else return (mkGenericPackageDescription (wrapperName ctx) modules)
     writeCabalFile cabalFile gpkg
  where
    cabalFile = targetPath ctx </> wrapperName ctx ++ ".cabal"
    modules   = map (Cabal.fromString . unModuleName) (sources ctx)
             ++ map (Cabal.fromString . qualModName . (namespace ctx ++)) (allSubResourceIds tree)

writeCabalFile :: FilePath -> Cabal.GenericPackageDescription -> IO ()
writeCabalFile path = Cabal.writeUTF8File path . unlines . filter emptyField . lines . Cabal.showGenericPackageDescription
  where emptyField = (/= "\"\" ") . takeWhile (/= ':') . reverse

updateExposedModules :: [Cabal.ModuleName] -> Cabal.GenericPackageDescription -> Cabal.GenericPackageDescription
updateExposedModules modules = modify _condLibrary (Just . maybe (mkCondLibrary modules) (set (_exposedModules . _condTreeData) modules))

mkGenericPackageDescription :: String -> [Cabal.ModuleName] -> Cabal.GenericPackageDescription
mkGenericPackageDescription name modules = Cabal.GenericPackageDescription pkg [] (Just (mkCondLibrary modules)) [] [] []
  where
    pkg = Cabal.emptyPackageDescription
      { Cabal.package        = Cabal.PackageIdentifier (Cabal.PackageName name) (Cabal.Version [0, 1] [])
      , Cabal.buildType      = Just Cabal.Simple
      , Cabal.specVersionRaw = Right (Cabal.orLaterVersion (Cabal.Version [1, 8] []))
      }

mkCondLibrary :: [Cabal.ModuleName] -> Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Library
mkCondLibrary modules = Cabal.CondNode
  { Cabal.condTreeData        = Cabal.Library modules True Cabal.emptyBuildInfo { Cabal.hsSourceDirs = ["src"] }
  , Cabal.condTreeConstraints =
     [ Cabal.Dependency (Cabal.PackageName "base")        (Cabal.withinVersion $ Cabal.Version [4]     [])
     , Cabal.Dependency (Cabal.PackageName "rest-types")  (Cabal.withinVersion $ Cabal.Version [1, 10] [])
     , Cabal.Dependency (Cabal.PackageName "rest-client") (Cabal.withinVersion $ Cabal.Version [0,  4] [])
     ]
  , Cabal.condTreeComponents  = []
  }

writeRes :: HaskellContext -> ApiResource -> IO ()
writeRes ctx node =
  do createDirectoryIfMissing True (targetPath ctx </> "src" </> modPath (namespace ctx ++ resParents node))
     writeFile (targetPath ctx </> "src" </> modPath (namespace ctx ++ resId node) ++ ".hs") (mkRes ctx node)

mkRes :: HaskellContext -> ApiResource -> String
mkRes ctx node = H.prettyPrint $ buildHaskellModule ctx node pragmas Nothing
  where
    pragmas = [ H.LanguagePragma noLoc [H.Ident "OverloadedStrings"],
                H.OptionsPragma noLoc (Just H.GHC) "-fno-warn-unused-imports"]
    _warningText = "Warning!! This is automatically generated code, do not modify!"

buildHaskellModule :: HaskellContext -> ApiResource ->
                      [H.ModulePragma] -> Maybe H.WarningText ->
                      H.Module
buildHaskellModule ctx node pragmas warningText =
  rewriteModuleNames (rewrites ctx) $
     H.Module noLoc name pragmas warningText exportSpecs importDecls decls
  where
    name = H.ModuleName $ qualModName $ namespace ctx ++ resId node
    exportSpecs = Nothing
    importDecls = nub $ namedImport "Rest.Client.Internal"
                      : extraImports
                     ++ parentImports
                     ++ dataImports
                     ++ idImports
    decls = idData node ++ concat funcs

    extraImports = imports ctx
    parentImports = map mkImport . tail . inits . resParents $ node
    dataImports = map (qualImport . unModuleName) datImp
    idImports = concat . mapMaybe (return . map (qualImport . unModuleName) . Ident.haskellModules <=< snd) . resAccessors $ node

    (funcs, datImp) = second (nub . concat) . unzip . map (mkFunction (apiVersion ctx) . resName $ node) $ resItems node
    mkImport p = (namedImport importName) { H.importQualified = True,
                                            H.importAs = importAs' }
      where importName = qualModName $ namespace ctx ++ p
            importAs' = fmap (H.ModuleName . modName) . lastMay $ p

rewriteModuleNames :: [(H.ModuleName, H.ModuleName)] -> H.Module -> H.Module
rewriteModuleNames rews = U.transformBi $ \m -> lookupJustDef m m rews

noBinds :: H.Binds
noBinds = H.BDecls []

use :: H.Name -> H.Exp
use = H.Var . H.UnQual

useMQual :: (Maybe H.ModuleName) -> H.Name -> H.Exp
useMQual Nothing = use
useMQual (Just qual) = H.Var . (H.Qual $ qual)

mkFunction :: Version -> String -> ApiAction -> ([H.Decl], [H.ModuleName])
mkFunction ver res (ApiAction _ lnk ai) =
  ([H.TypeSig noLoc [funName] fType,
    H.FunBind [H.Match noLoc funName fParams Nothing rhs noBinds]],
   errorInfoModules errorI ++ infoModules output ++ maybe [] infoModules mInp)
     where
       funName = mkHsName ai
       fParams = map H.PVar $ lPars
                           ++ maybe [] ((:[]) . hsName . cleanName . description) (ident ai)
                           ++ maybe [] (const [input]) mInp
                           ++ (if null (params ai) then [] else [pList])
       (lUrl, lPars) = linkToURL res lnk
       mInp    = fmap inputInfo . chooseType . inputs $ ai
       fType   = H.TyForall Nothing [H.ClassA (H.UnQual cls) [m]] $ fTypify tyParts
         where cls = H.Ident "ApiStateC"
               m = H.TyVar $ H.Ident "m"
               fTypify :: [H.Type] -> H.Type
               fTypify [] = error "Rest.Gen.Haskell.mkFunction.fTypify - expects at least one type"
               fTypify [ty1] = ty1
               fTypify [ty1, ty2] = H.TyFun ty1 ty2
               fTypify (ty1 : tys) = H.TyFun ty1 (fTypify tys)
               tyParts = map qualIdent lPars
                         ++ maybe [] (return . Ident.haskellType) (ident ai)
                         ++ inp
                         ++ (if null (params ai) then []
                             else [H.TyList (H.TyTuple H.Boxed [haskellStringType,
                                                                haskellStringType])])
                         ++ [H.TyApp m (H.TyApp
                                         (H.TyApp
                                           (H.TyCon $ H.UnQual (H.Ident "ApiResponse"))
                                           (maybe haskellUnitType id (errorInfoType errorI)))
                                         (maybe haskellUnitType id (infoType output)))]
               qualIdent (H.Ident s)
                 | s == res = H.TyCon $ H.UnQual tyIdent
                 | otherwise = H.TyCon $ H.Qual (H.ModuleName $ modName s) tyIdent
               qualIdent H.Symbol{} = error "Rest.Gen.Haskell.mkFunction.qualIdent - not expecting a Symbol"
               inp | Just i <- mInp,
                     Just i' <- infoType i = [i']
                   | otherwise = []
       input = H.Ident "input"
       pList = H.Ident "pList"
       rhs = H.UnGuardedRhs $ H.Let binds expr
         where binds = H.BDecls [rHeadersBind, requestBind]
               rHeadersBind =
                 H.PatBind noLoc (H.PVar rHeaders) Nothing
                    (H.UnGuardedRhs $ H.List [H.Tuple H.Boxed [use hAccept, H.Lit $ H.String $ infoContentType output],
                                              H.Tuple H.Boxed [use hContentType, H.Lit $ H.String $ maybe "text/plain" infoContentType mInp]])
                              noBinds

               rHeaders     = H.Ident "rHeaders"
               hAccept      = H.Ident "hAccept"
               hContentType = H.Ident "hContentType"
               doRequest    = H.Ident "doRequest"

               requestBind =
                 H.PatBind noLoc (H.PVar request) Nothing
                    (H.UnGuardedRhs $
                      appLast
                        (H.App
                          (H.App
                            (H.App
                              (H.App (H.App (use makeReq) (H.Lit $ H.String $ show $ method ai))
                                     (H.Lit $ H.String ve))
                              url)
                            (if null (params ai) then (H.List []) else (use pList)))
                          (use rHeaders))) noBinds
               appLast e
                 | Just i <- mInp = H.App e (H.App (use $ H.Ident $ infoFunc i) (use input))
                 | otherwise = H.App e (H.Lit $ H.String "")
               makeReq = H.Ident "makeReq"
               request = H.Ident "request"

               expr = H.App (H.App (H.App (use doRequest)
                                          (use $ H.Ident $ errorInfoFunc errorI))
                                          (use $ H.Ident $ infoFunc output)) (use request)

       (ve, url) = ("v" ++ show ver, lUrl)
       errorI = headDef (ErrorInfo [] (Just haskellUnitType) defaultErrorConversion)
                . map errorInfo
                . mapMaybe (\v -> find ((v ==) . dataType) $ errors ai)
                $ maybeToList (dataType <$> chooseType (outputs ai)) ++ [XML, JSON]
       output = maybe (Info [] (Just haskellUnitType) "text/plain" "(const ())") outputInfo . chooseType . outputs $ ai
       defaultErrorConversion = if fmap dataType (chooseType (outputs ai)) == Just JSON then "fromJSON" else "fromXML"

linkToURL :: String -> Link -> (H.Exp, [H.Name])
linkToURL res lnk = first H.List $ urlParts res lnk ([], [])

urlParts :: String -> Link -> ([H.Exp], [H.Name]) -> ([H.Exp], [H.Name])
urlParts res lnk ac@(rlnk, pars) =
  case lnk of
    [] -> ac
    (LResource r : a@(LAccess _) : xs)
      | not (hasParam a) -> urlParts res xs (rlnk ++ [H.List [H.Lit $ H.String r]], pars)
      | otherwise -> urlParts res xs (rlnk', pars ++ [H.Ident r])
           where rlnk' = rlnk ++ (H.List [H.Lit $ H.String $ r] : tailed)
                 tailed = [H.App (useMQual qual $ H.Ident "readId")
                                 (use $ hsName (cleanName r))]
                   where qual | r == res = Nothing
                              | otherwise = Just $ H.ModuleName $ modName r
    (LParam p : xs) -> urlParts res xs (rlnk ++ [H.List [H.App (use $ H.Ident "showUrl")
                                                          (use $ hsName (cleanName p))]], pars)
    (i : xs) -> urlParts res xs (rlnk ++ [H.List [H.Lit $ H.String $ itemString i]], pars)

idData :: ApiResource -> [H.Decl]
idData node =
  case resAccessors node of
    [] -> []
    [(_pth, Nothing)] -> []
    [(pth, Just i)] ->
      let pp xs | null pth = xs
                | otherwise = H.Lit (H.String pth) : xs
      in [ H.TypeDecl noLoc tyIdent [] (Ident.haskellType i),
           H.TypeSig noLoc [funName] fType,
           H.FunBind [ H.Match noLoc funName [H.PVar x] Nothing
                       (H.UnGuardedRhs $ H.List $ pp [ showURLx ]) noBinds] ]
    ls ->
      let ctor (pth,mi) =
            H.QualConDecl noLoc [] [] (H.ConDecl (H.Ident (dataName pth)) $ maybe [] f mi)
              where f ty = [H.UnBangedTy $ Ident.haskellType ty]
          fun (pth, mi) = [
                           H.FunBind [H.Match noLoc funName fparams Nothing rhs noBinds]]
            where (fparams, rhs) =
                    case mi of
                      Nothing ->
                        ([H.PVar $ H.Ident (dataName pth)],
                         (H.UnGuardedRhs $ H.List [H.Lit (H.String pth)]))
                      Just{}  ->  -- Pattern match with data constructor
                        ([H.PParen $ H.PApp (H.UnQual $ H.Ident (dataName pth)) [H.PVar x]],
                         (H.UnGuardedRhs $ H.List [H.Lit $ H.String pth, showURLx]))
      in [ H.DataDecl noLoc H.DataType [] tyIdent [] (map ctor ls) []
         , H.TypeSig noLoc [funName] fType
         ] ++ concatMap fun ls
    where
      x        = H.Ident "x"
      fType    = H.TyFun (H.TyCon $ H.UnQual tyIdent) (H.TyList haskellStringType)
      funName  = H.Ident "readId"
      showURLx = H.App (H.Var $ H.UnQual $ H.Ident "showUrl") (H.Var $ H.UnQual $ x)

tyIdent :: H.Name
tyIdent = H.Ident "Identifier"

mkHsName :: ActionInfo -> H.Name
mkHsName ai = hsName $ concatMap cleanName parts
  where
      parts = case actionType ai of
                Retrieve   -> let nm = get ++ by ++ target
                              in if null nm then ["access"] else nm
                Create     -> ["create"] ++ by ++ target
                -- Should be delete, but delete is a JS keyword and causes problems in collect.
                Delete     -> ["remove"] ++ by ++ target
                DeleteMany -> ["removeMany"] ++ by ++ target
                List       -> ["list"]   ++ by ++ target
                Update     -> ["save"]   ++ by ++ target
                UpdateMany -> ["saveMany"] ++ by ++ target
                Modify   -> if resDir ai == "" then ["do"] else [resDir ai]

      target = if resDir ai == "" then maybe [] ((:[]) . description) (ident ai) else [resDir ai]
      by     = if target /= [] && (isJust (ident ai) || actionType ai == UpdateMany) then ["by"] else []
      get    = if isAccessor ai then [] else ["get"]

hsName :: [String] -> H.Name
hsName []       = H.Ident ""
hsName (x : xs) = H.Ident $ clean $ downFirst x ++ concatMap upFirst xs
  where
    clean s = if s `elem` reservedNames then s ++ "_" else s
    reservedNames =
      ["as","case","class","data","instance","default","deriving","do"
      ,"foreign","if","then","else","import","infix","infixl","infixr","let"
      ,"in","module","newtype","of","qualified","type","where"]

qualModName :: ResourceId -> String
qualModName = intercalate "." . map modName

modPath :: ResourceId -> String
modPath = intercalate "/" . map modName

dataName :: String -> String
dataName = modName

modName :: String -> String
modName = concatMap upFirst . cleanName

data Info = Info
  { infoModules     :: [H.ModuleName]
  , infoType        :: Maybe H.Type
  , infoContentType :: String
  , infoFunc        :: String
  } deriving (Eq, Show)

inputInfo :: DataDescription -> Info
inputInfo ds =
  case dataType ds of
    String -> Info []                  (Just haskellStringType)     "text/plain"               "fromString"
    XML    -> Info (haskellModules ds) (haskellType ds)             "text/xml"                 "toXML"
    JSON   -> Info (haskellModules ds) (haskellType ds)             "text/json"                "toJSON"
    File   -> Info []                  (Just haskellByteStringType) "application/octet-stream" "id"
    Other  -> Info []                  (Just haskellByteStringType) "text/plain"               "id"

outputInfo :: DataDescription -> Info
outputInfo ds =
  case dataType ds of
    String -> Info []                  (Just haskellStringType)     "text/plain" "toString"
    XML    -> Info (haskellModules ds) (haskellType ds)             "text/xml"   "fromXML"
    JSON   -> Info (haskellModules ds) (haskellType ds)             "text/json"  "fromJSON"
    File   -> Info []                  (Just haskellByteStringType) "*"          "id"
    Other  -> Info []                  (Just haskellByteStringType)"text/plain" "id"

data ErrorInfo = ErrorInfo
  { errorInfoModules :: [H.ModuleName]
  , errorInfoType    :: (Maybe H.Type)
  , errorInfoFunc    :: String
  } deriving (Eq, Show)

errorInfo :: DataDescription -> ErrorInfo
errorInfo ds =
  case dataType ds of
    String -> ErrorInfo (haskellModules ds) (haskellType ds) "fromXML"
    XML    -> ErrorInfo (haskellModules ds) (haskellType ds) "fromXML"
    JSON   -> ErrorInfo (haskellModules ds) (haskellType ds) "fromJSON"
    File   -> ErrorInfo (haskellModules ds) (haskellType ds) "fromXML"
    Other  -> ErrorInfo (haskellModules ds) (haskellType ds) "fromXML"
