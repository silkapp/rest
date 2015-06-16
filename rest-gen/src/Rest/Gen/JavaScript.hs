{-# LANGUAGE ScopedTypeVariables #-}
module Rest.Gen.JavaScript (mkJsApi) where

import Prelude hiding ((.))

import Control.Category ((.))
import Control.Monad
import Data.Maybe
import Text.StringTemplate
import qualified Data.Label.Total             as L
import qualified Data.List.NonEmpty           as NList
import qualified Language.Haskell.Exts.Syntax as H

import Code.Build
import Code.Build.JavaScript
import Rest.Api (Router, Version)
import Rest.Gen.Base
import Rest.Gen.Types
import Rest.Gen.Utils

mkJsApi :: H.ModuleName -> Bool -> Version -> Router m s -> IO String
mkJsApi ns priv ver r =
  do prelude <- liftM (render . setManyAttrib attrs . newSTMP) (readContent "Javascript/base.js")
     let cod = showCode $ mkStack
                [ unModuleName ns ++ ".prototype.version" .=. string (show ver)
                , mkJsCode (unModuleName ns) priv r
                ]
     return $ mkJsModule (prelude ++ cod)
  where attrs = [("apinamespace", unModuleName ns), ("dollar", "$")]

mkJsModule :: String -> String
mkJsModule content = "(function (window) {\n\n" ++ content ++ "\n\n})(this);"

mkJsCode :: String -> Bool -> Router m s -> Code
mkJsCode ns priv = mkJs ns . sortTree . (if priv then id else noPrivate) . apiSubtrees

mkJs :: String -> ApiResource -> Code
mkJs ns = foldTreeChildren mkStack (\i ls -> mkStack $ mkRes ns i : ls)

mkRes :: String -> ApiResource -> Code
mkRes ns node = mkStack $
  [ if hasAccessor node
      then resourceLoc ns node .=. mkAccessorConstructor ns node
      else resourceLoc ns node .=. jsObject []
  , resourceLoc ns node ++ ".apiObjectType" .=. string "resourceDir"
  , mkAccessFuncs ns node
  , mkPreFuncs ns node
  , mkPostFuncs ns node
  ]

mkAccessorConstructor :: String -> ApiResource -> Code
mkAccessorConstructor ns resource =
  let constrName = jsDir (cleanName (resName resource))
  in functionDecl constrName ["url", "secureUrl", "modifyRequest"] $
       jsIf (code $ "this instanceof " ++ constrName)
            (proc (ns ++ ".setContext") (code "this, url, secureUrl, modifyRequest"))
         <-> jsElse (ret $ call (constrName ++ ".access") (code "url, secureUrl, modifyRequest"))

mkPreFuncs :: String -> ApiResource -> Code
mkPreFuncs ns node =
  let items = filter ((\i -> not $ postAction i || isAccessor i) . itemInfo) $ resItems node
  in mkFunctions (resourceLoc ns node ++ ".") (mkFunction ns) items

mkAccessFuncs :: String -> ApiResource -> Code
mkAccessFuncs ns node =
  let items = filter ((\i -> not (postAction i) && isAccessor i) . itemInfo) $ resItems node
  in mkFunctions (resourceLoc ns node ++ ".") (mkAccessor ns) items

mkPostFuncs :: String -> ApiResource -> Code
mkPostFuncs ns node =
  let items = filter (postAction . itemInfo) $ resItems node
  in mkFunctions (resourceLoc ns node ++ ".prototype.") (mkFunction ns) items

mkFunctions :: String -> (ApiAction -> Code) -> [ApiAction] -> Code
mkFunctions loc maker = mkStack . map (\item -> loc ++ mkJsName item .=. maker item)

mkAccessor :: String -> ApiAction -> Code
mkAccessor ns node@(ApiAction _ _ ai) =
  let fParams  = maybeToList mIdent
      urlPart  = (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + encodeURIComponent(" ++ i ++ ") + '/") mIdent
      mIdent   = fmap (jsId . cleanName . description) $ ident ai
  in function fParams $
      [ var "postfix" $ "'" ++ urlPart ++ "'"
      , var "accessor" $ new "this" . code $  "this.contextUrl + postfix, "
                                           ++ "this.secureContextUrl + postfix, "
                                           ++ "this.modifyRequest"
      , "accessor.get" .=. mkFunction ns node
      , ret "accessor"
      ]

mkFunction :: String -> ApiAction -> Code
mkFunction ns (ApiAction _ _ ai) =
  let fParams  = maybeToList mIdent
              ++ maybeToList (fmap fst3 mInp)
              ++ ["success", "error", "params", "callOpts"]
      mInp     = fmap (mkType . L.get (dataType . desc) . chooseType) . NList.nonEmpty . inputs $ ai
      mOut     = dataTypesToAcceptHeader JSON . responseAcceptType . chooseResponseType $ ai
      urlPart  = (if isAccessor ai then const "" else id) $
                 (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + encodeURIComponent(" ++ i ++ ") + '/") mIdent
      mIdent   = (if isAccessor ai then const Nothing else id) $ fmap (jsId . cleanName . description) $ ident ai
  in function fParams $ ret $
        call (ns ++ "." ++ "ajaxCall")
          [ string (method ai)
          , code $ (if (https ai) then "this.secureContextUrl" else "this.contextUrl") ++ " + '" ++ urlPart ++ "'"
          , code "params"
          , code "success"
          , code "error"
          , string $ maybe "text/plain" snd3 mInp
          , string $ mOut
          , maybe (code "undefined") (\(p, _, f) -> f (code p)) mInp
          , code "callOpts"
          , code "this.modifyRequest"
          ]

resourceLoc :: String -> ApiResource -> String
resourceLoc ns = ((ns ++ ".prototype.") ++) . locFromLink . resLink
  where locFromLink (LResource i1 : LAccess [] : LResource i2 : xs) = jsDir (cleanName i1) ++ "." ++ locFromLink (LResource i2 : xs)
        locFromLink (LResource i : xs) = case locFromLink xs of
                                          [] -> jsDir $ cleanName i
                                          ls -> jsDir (cleanName i) ++ ".prototype." ++ ls
        locFromLink (_ : xs) = locFromLink xs
        locFromLink [] = ""

mkJsName :: ApiAction -> String
mkJsName item =
  case mkFuncParts item of
    []       -> ""
    (x : xs) -> x ++ concatMap upFirst xs

jsDir :: [String] -> String
jsDir = concatMap upFirst

jsId :: [String] -> String
jsId []       = ""
jsId (x : xs) = x ++ concatMap upFirst xs

mkType :: DataType -> (String, String, Code -> Code)
mkType dt = (dataTypeString dt, dataTypeToAcceptHeader dt, fn)
  where
    fn = case dt of
      String -> id
      XML    -> id
      JSON   -> call "JSON.stringify"
      File   -> id
      Other  -> id
