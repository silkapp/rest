{-# LANGUAGE
    DoAndIfThenElse
  , TemplateHaskell
  #-}
module Rest.Gen.Haskell.Generate
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
import qualified Distribution.ModuleName                     as Cabal
import qualified Distribution.Package                        as Cabal
import qualified Distribution.PackageDescription             as Cabal
import qualified Distribution.PackageDescription.Parse       as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Simple.Utils                   as Cabal
import qualified Distribution.Verbosity                      as Cabal
import qualified Distribution.Version                        as Cabal

import Code.Build
import Code.Build.Haskell
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
    , sources        :: [ModuleName]
    , imports        :: [Import]
    , rewrites       :: [(ModuleName, ModuleName)]
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
  { Cabal.condTreeData        = Cabal.Library modules True Cabal.emptyBuildInfo
  , Cabal.condTreeConstraints = []
  , Cabal.condTreeComponents  = []
  }

writeRes :: HaskellContext -> ApiResource -> IO ()
writeRes ctx node =
  do createDirectoryIfMissing True (targetPath ctx </> "src" </> modPath (namespace ctx ++ resParents node))
     writeFile (targetPath ctx </> "src" </> modPath (namespace ctx ++ resId node) ++ ".hs") (mkRes ctx node)

mkRes :: HaskellContext -> ApiResource -> String
mkRes ctx node =
  showCode $
      "{-# LANGUAGE OverloadedStrings #-}\n{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n{- Warning!! This is automatically generated code, do not modify! -}"
  <-> hsModule (qualModName $ namespace ctx ++ resId node)
       [ mkImports ctx node mods
       , idData node
       , mkStack funcs
       ]
  where
    (funcs, mods) = second (nub . concat) . unzip . map (mkFunction (apiVersion ctx) . resName $ node) $ resItems node

mkImports :: HaskellContext -> ApiResource -> [ModuleName] -> Code
mkImports ctx node datImp
  = mkStack . nub . map (rewriteImport $ rewrites ctx)
  $    [Import UnQualified (ModuleName "Rest.Client.Internal") Nothing Nothing]
    ++ extraImports
    ++ parentImports
    ++ dataImports
    ++ idImports
  where
    extraImports  = imports ctx
    parentImports = map mkImport . tail . inits . resParents $ node
    dataImports   = map qualImp datImp
    idImports     = concat . mapMaybe (return . map qualImp . Ident.haskellModules <=< snd) . resAccessors $ node
    -- We need the `as' name to be explicit here even though it's the same, see comment below.
    qualImp v     = Import Qualified v (Just v) Nothing
    mkImport p    = Import Qualified (ModuleName . qualModName $ namespace ctx ++ p) (fmap (ModuleName . modName) . lastMay $ p) Nothing
    rewriteImport :: [(ModuleName, ModuleName)] -> Import -> Import
    rewriteImport rws i = case i of
      -- We don't rewrite the `as` part of the import so if you have a
      -- rewrite ("Data.Text.Internal.Lazy", "Data.Text.Lazy") the
      -- import will become `import qualified Data.Text as
      -- Data.Text.Internal.Lazy', this is because mkFunction produces
      -- types through strings and doesn't take rewrites into
      -- account. mkFunction should be changed to do this.
      Import q m mas l -> Import q (look m) mas l
      where
       look m = lookupJustDef m m rws

mkFunction :: Version -> String -> ApiAction -> (Code, [ModuleName])
mkFunction ver res (ApiAction _ lnk ai) =
  let mInp     = fmap inputInfo . chooseType . inputs $ ai
      defaultErrorConversion = if fmap dataType (chooseType (outputs ai)) == Just JSON then "fromJSON" else "fromXML"
      output {-(oMod, oType, oCType, oFunc)-} = maybe (Info [] "()" "text/plain" "(const ())") outputInfo $ chooseType $ outputs ai
      (eMod, eType, eFunc) = headDef ([], "()", defaultErrorConversion)
                           . map errorInfo
                           . mapMaybe (\v -> find ((v ==) . dataType) $ errors ai)
                           $ maybeToList (dataType <$> chooseType (outputs ai)) ++ [XML, JSON]
      (lUrl, lPars) = linkToURL res lnk
      (ve, url)     = (string $ "v" <+> show ver, lUrl)
      fParams  = map (hsName . cleanName) lPars
              ++ maybe [] ((:[]) . hsName . cleanName . description) (ident ai)
              ++ maybe [] (const ["input"]) mInp
              ++ (if null (params ai) then [] else ["pList"])
      fType    = "ApiStateC m => "
              ++ hsType (map (\p -> (if p == res then "" else modName p ++ ".") ++ "Identifier") lPars
                      ++ maybe [] (return . Ident.haskellType) (ident ai)
                      ++ maybe [] ((:[]) . infoType) mInp
                      ++ (if null (params ai) then [] else ["[(String, String)]"])
                      ++ ["m (ApiResponse (" ++ eType ++ ") (" ++ infoType output ++ "))"])
  in ( mkStack
        [ function (mkHsName ai) fType
        , hsDecl (mkHsName ai) fParams $
           hsLet
              [ "rHeaders" .=. hsArray [ hsTuple [code "hAccept", string (infoContentType output)]
                                       , hsTuple [code "hContentType", string (maybe "text/plain" infoContentType mInp)]
                                       ]
              , "request" .=. "makeReq"
                                  <++> string (show (method ai))
                                  <++> ve
                                  <++> url
                                  <++> (if null (params ai) then "[]" else "pList")
                                  <++> "rHeaders"
                                  <++> "$"
                                  <++> maybe "\"\"" ((++ " input") . infoFunc) mInp
              ]
              $ "doRequest " <++> eFunc <++> infoFunc output <++> "request"
        ]
     , eMod ++ infoModules output ++ maybe [] infoModules mInp
     )

linkToURL :: String -> Link -> (Code, [String])
linkToURL res lnk = first hsArray $ urlParts res lnk ([], [])

urlParts :: String -> Link -> ([Code], [String]) -> ([Code], [String])
urlParts res lnk ac@(rlnk, pars) =
  case lnk of
    [] -> ac
    (LResource r : a@(LAccess _) : xs) | not (hasParam a) -> urlParts res xs (rlnk ++ [hsArray [string r]], pars)
                                       | otherwise ->
      urlParts res xs
            ( rlnk ++ [ hsArray [string r]
                      , (if r == res then noCode else modName r <+> ".") <+> "readId" <++> hsName (cleanName r)
                      ]
            , pars ++ [r]
            )
    (LParam p : xs) -> urlParts res xs (rlnk ++ [hsArray ["showUrl" <++> hsName (cleanName p)]], pars)
    (i : xs) -> urlParts res xs (rlnk ++ [hsArray [string $ itemString i]], pars)

idData :: ApiResource -> Code
idData node =
  case resAccessors node of
    []  -> noCode
    [(pth,mi)] -> maybe noCode
           (\i -> mkStack
            [ code "type Identifier = " <++> Ident.haskellType i
            , function "readId" "Identifier -> [String]"
            , hsDecl "readId" ["x"] (hsArray $ if pth /= ""
                                               then [string pth, code "showUrl x"]
                                               else [code "showUrl x"]
                                    )
            ]
           )
           mi
    ls  -> mkStack
            [ hsData "Identifier" $ map (\(pth,mi) -> dataName pth ++ maybe "" (\x -> " (" ++ Ident.haskellType x ++ ")") mi) ls
            , function "readId" "Identifier -> [String]"
            , mkStack $
                map (\(pth,mi) ->
                        if isJust mi
                          then hsDecl "readId" ["(" ++ dataName pth ++ " x" ++ ")"] $ hsArray [string pth, code "showUrl x"]
                          else hsDecl "readId" [dataName pth] $ hsArray [string pth]
                    ) ls
            ]

mkHsName :: ActionInfo -> String
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

hsName :: [String] -> String
hsName []       = ""
hsName (x : xs) = clean $ downFirst x ++ concatMap upFirst xs
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

modName :: String -> String
modName = concatMap upFirst . cleanName

dataName :: String -> String
dataName = modName

data Info = Info
  { infoModules     :: [ModuleName]
  , infoType        :: String
  , infoContentType :: String
  , infoFunc        :: String
  }

inputInfo :: DataDescription -> Info
inputInfo ds =
  case dataType ds of
    String -> Info [] "String" "text/plain" "fromString"
    XML    -> Info (haskellModules ds) (haskellType ds) "text/xml" "toXML"
    JSON   -> Info (haskellModules ds) (haskellType ds) "text/json" "toJSON"
    File   -> Info [] "ByteString" "application/octet-stream" "id"
    Other  -> Info [] "ByteString" "text/plain" "id"

outputInfo :: DataDescription -> Info
outputInfo ds =
  case dataType ds of
    String -> Info [] "String" "text/plain" "toString"
    XML    -> Info (haskellModules ds) (haskellType ds) "text/xml" "fromXML"
    JSON   -> Info (haskellModules ds) (haskellType ds) "text/json" "fromJSON"
    File   -> Info [] "ByteString" "*" "id"
    Other  -> Info [] "ByteString" "text/plain" "id"

errorInfo :: DataDescription -> ([ModuleName], String, String)
errorInfo ds =
  case dataType ds of
    String -> (haskellModules ds, haskellType ds, "fromXML")
    XML    -> (haskellModules ds, haskellType ds, "fromXML")
    JSON   -> (haskellModules ds, haskellType ds, "fromJSON")
    File   -> (haskellModules ds, haskellType ds, "fromXML")
    Other  -> (haskellModules ds, haskellType ds, "fromXML")
