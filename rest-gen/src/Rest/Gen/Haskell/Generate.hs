{-# LANGUAGE DoAndIfThenElse, TemplateHaskell #-}
module Rest.Gen.Haskell.Generate where

import Control.Applicative
import Control.Arrow (first)
import Control.Category
import Control.Monad
import Data.Label (mkLabels, modify, set)
import Data.List
import Data.Maybe
import Prelude hiding ((.), id)
import Safe
import System.Directory
import System.FilePath

import qualified Distribution.ModuleName                     as Cabal
import qualified Distribution.Package                        as Cabal
import qualified Distribution.PackageDescription             as Cabal
import qualified Distribution.PackageDescription.Parse       as Cabal
import qualified Distribution.Verbosity                      as Cabal
import qualified Distribution.Version                        as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Simple.Utils                   as Cabal

import Code.Build
import Code.Build.Haskell
import Rest.Api (Version, Router)

import Rest.Gen.Base
import Rest.Gen.Utils

import qualified Rest.Gen.Base.ActionInfo.Ident as Ident

$(mkLabels[''Cabal.GenericPackageDescription, ''Cabal.CondTree, ''Cabal.Library])

data HaskellContext =
  HaskellContext
    { apiVersion     :: Version
    , targetPath     :: String
    , wrapperName    :: String
    , includePrivate :: Bool
    , sources        :: [String]
    , imports        :: [String]
    , rewrites       :: [(String, String)]
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
    modules   = map Cabal.fromString (sources ctx)
             ++ map (Cabal.fromString . qualModName . (namespace ctx ++)) (allSubResourceIds tree)

writeCabalFile :: FilePath -> Cabal.GenericPackageDescription -> IO ()
writeCabalFile path = Cabal.writeUTF8File path . unlines . filter emptyField . lines . Cabal.showGenericPackageDescription
  where emptyField = (/= "\"\" ") . takeWhile (/= ':') . reverse

updateExposedModules :: [Cabal.ModuleName] -> Cabal.GenericPackageDescription -> Cabal.GenericPackageDescription
updateExposedModules modules = modify lCondLibrary (Just . maybe (mkCondLibrary modules) (set (lExposedModules . lCondTreeData) modules))

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
       [ mkImports ctx node $ rewriteModules (rewrites ctx) $ nub $ concat mods
       , idData node
       , mkStack funcs
       ]
  where (funcs, mods) = unzip $ map (mkFunction (apiVersion ctx) $ resName node) $ resItems node

mkImports :: HaskellContext -> ApiResource -> [String] -> Code
mkImports ctx node datImp =
    mkStack
      [ code "import Rest.Client.Internal"
      , extraImports
      , parentImports
      , dataImports
      ]
  where
    extraImports  = mkStack . map ("import " <+>) $ imports ctx
    parentImports = mkStack . map mkImport . tail . inits . resParents $ node
    dataImports   = mkStack . map ("import qualified " <+>) $ datImp
    mkImport p = "import qualified" <++> qualModName (namespace ctx ++ p) <++> "as" <++> modName (last p)

mkFunction :: Version -> String -> ApiAction -> (Code, [String])
mkFunction ver res (ApiAction _ lnk ai) =
  let mInp     = fmap inputInfo $ chooseType $ inputs ai
      identMod = maybe [] Ident.haskellModule (ident ai)
      defaultErrorConversion = if fmap dataType (chooseType (outputs ai)) == Just JSON then "fromJSON" else "fromXML"
      (oMod, oType, oCType, oFunc) = maybe ([], "()", "text/plain", "(const ())") outputInfo $ chooseType $ outputs ai
      (eMod, eType, eFunc) = headDef (([], "()", defaultErrorConversion))
                           . map errorInfo
                           . catMaybes
                           . map (\v -> find ((v ==) . dataType) $ errors ai)
                           $ maybeToList (fmap dataType $ chooseType (outputs ai)) ++ [XML, JSON]
      (lUrl, lPars) = linkToURL res lnk
      url      = string ("v" <+> show ver <+> "/") <++> "++" <++> lUrl
      fParams  = map (hsName . cleanName) lPars
              ++ maybe [] ((:[]) . hsName . cleanName . description) (ident ai)
              ++ maybe [] (const ["input"]) mInp
              ++ (if null (params ai) then [] else ["pList"])
      fType    = "ApiStateC m => "
              ++ (hsType $ map (\p -> (if p == res then "" else modName p ++ ".") ++ "Identifier") lPars
                       ++ maybe [] (return . Ident.haskellType) (ident ai)
                       ++ maybe [] (\(_,v,_,_) -> [v]) mInp
                       ++ (if null (params ai) then [] else ["[(String, String)]"])
                       ++ ["m (ApiResponse (" ++ eType ++ ") (" ++ oType ++ "))"])
  in ( mkStack $
        [ function (mkHsName ai) fType
        , hsDecl (mkHsName ai) fParams $
           hsLet
              [ "rHeaders" .=. hsArray [ hsTuple [code "hAccept", string oCType]
                                       , hsTuple [code "hContentType", string (maybe "text/plain" (\(_,_,v,_) -> v) mInp)]
                                       ]
              , "request" .=. "ApiRequest"
                                  <++> string (show (method ai))
                                  <++> parenthesis url
                                  <++> (if null (params ai) then "[]" else "pList")
                                  <++> "rHeaders"
                                  <++> "$"
                                  <++> maybe "\"\"" ((++ " input") . (\(_,_,_,v) -> v)) mInp
              ]
              $ "liftM (parseResult" <++> eFunc <++> oFunc <+> ") . doRequest $ request"
        ]
     , eMod ++ oMod ++ identMod ++ maybe [] (\(m,_,_,_) -> m) mInp
     )

linkToURL :: String -> Link -> (Code, [String])
linkToURL res lnk = first (\v -> "intercalate" <++> string "/" <++> parenthesis ("map encode $ concat" <++> hsArray v)) $ urlParts res lnk ([], [])

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
  let items  = filter isAccessor $ map itemInfo $ resItems node
      mkCons = dataName . resDir
  in case items of
      []  -> noCode
      [x] -> maybe noCode
             (\i -> mkStack
              [ code "type Identifier = " <++> Ident.haskellType i
              , function "readId" "Identifier -> [String]"
              , hsDecl "readId" ["x"] (hsArray $ if resDir x /= "" then [string $ resDir x, code "showUrl x"] else [code "showUrl x"])
              ]
             )
             (ident x)
      ls  -> mkStack $
              [ hsData "Identifier" $ map (\i -> mkCons i ++ maybe "" (\x -> " (" ++ Ident.haskellType x ++ ")") (ident i)) ls
              , function "readId" "Identifier -> [String]"
              , mkStack $
                  map (\i ->
                          if isJust (ident i)
                            then hsDecl "readId" ["(" ++ mkCons i ++ " x" ++ ")"] $ hsArray [string (resDir i), code "showUrl x"]
                            else hsDecl "readId" [mkCons i] $ hsArray [string (resDir i)]
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
                List       -> ["list"]   ++ by ++ target
                Update     -> ["save"]   ++ by ++ target
                UpdateMany -> ["saveMany"] ++ by ++ target
                Modify   -> if resDir ai == "" then ["do"] else [resDir ai]

      target = if resDir ai == "" then maybe [] ((:[]) . description) (ident ai) else [resDir ai]
      by     = if target /= [] && (isJust (ident ai) || actionType ai == UpdateMany) then ["by"] else []
      get    = if isAccessor ai then [] else ["get"]

rewriteModules :: [(String, String)] -> [String] -> [String]
rewriteModules _  [] = []
rewriteModules rw (v : vs) = maybe v (++ (" as " ++ v)) (lookup v rw) : rewriteModules rw vs

hsName :: [String] -> String
hsName []       = ""
hsName (x : xs) = downFirst x ++ concatMap upFirst xs

qualModName :: ResourceId -> String
qualModName = intercalate "." . map modName

modPath :: ResourceId -> String
modPath = intercalate "/" . map modName

modName :: String -> String
modName = concatMap upFirst . cleanName

dataName :: String -> String
dataName = modName

inputInfo :: DataDescription -> ([String], String, String, String)
inputInfo ds =
  case dataType ds of
    JSON  -> (haskellModule ds, haskellType ds, "text/json", "toJSON")
    XML   -> (haskellModule ds, haskellType ds, "text/xml", "toXML")
    File  -> ([], "ByteString", "application/octet-stream", "id")
    Other -> ([], "ByteString", "text/plain", "id")

outputInfo :: DataDescription -> ([String], String, String, String)
outputInfo ds =
  case dataType ds of
    JSON  -> (haskellModule ds, haskellType ds, "text/json", "fromJSON")
    XML   -> (haskellModule ds, haskellType ds, "text/xml", "fromXML")
    File  -> ([], "ByteString", "*", "id")
    Other -> ([], "ByteString", "text/plain", "id")

errorInfo :: DataDescription -> ([String], String, String)
errorInfo ds =
  case dataType ds of
    JSON  -> (haskellModule ds, haskellType ds, "fromJSON")
    XML   -> (haskellModule ds, haskellType ds, "fromXML")
    File  -> (haskellModule ds, haskellType ds, "fromXML")
    Other -> (haskellModule ds, haskellType ds, "fromXML")
