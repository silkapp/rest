{-# LANGUAGE ScopedTypeVariables #-}
module Rest.Gen.JavaScript.Generate (mkJsApi) where

import Code.Build
import Code.Build.JavaScript

import Control.Monad
import Data.Maybe
import Text.StringTemplate

import Rest.Api (Version, Router)
import Rest.Gen.Base
import Rest.Gen.Utils

mkJsApi :: String -> Bool -> Version -> Router m s -> IO String
mkJsApi ns priv ver r =
  do prelude <- liftM (render . setManyAttrib attrs . newSTMP) (readContent "Javascript/base.js")
     let cod = showCode $ mkStack
                [ ns ++ ".prototype.version" .=. string (show ver)
                , mkJsCode ns priv r
                ]
     return $ prelude ++ cod
  where attrs = [("apinamespace", ns), ("dollar", "$")]

mkJsCode :: String -> Bool -> Router m s -> Code
mkJsCode ns priv = mkJs ns . sortTree . (if priv then id else noPrivate) . apiSubtrees

mkJs :: String -> ApiResource -> Code
mkJs ns = foldTreeChildren mkStack (\i ls -> mkStack $ mkRes ns i : ls)

mkRes :: String -> ApiResource -> Code
mkRes ns node = mkStack $
  [ if hasAccessor node
      then resourceLoc ns node .=. ns ++ ".makeSilkConstructor()"
      else resourceLoc ns node .=. jsObject []
  , resourceLoc ns node ++ ".apiObjectType" .=. string "resourceDir"
  , mkAccessFuncs ns node
  , mkPreFuncs ns node
  , mkPostFuncs ns node
  ]

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
      [ var "accessor" $ new "this" . code $ "this.contextUrl + '" ++ urlPart ++ "'"
      , "accessor.get" .=. mkFunction ns node
      , ret "accessor"
      ]

mkFunction :: String -> ApiAction -> Code
mkFunction ns (ApiAction _ _ ai) =
  let fParams  = maybeToList mIdent
              ++ maybeToList (fmap fst3 mInp)
              ++ ["success", "error", "params", "callOpts"]
      mInp     = fmap mkType . chooseType $ inputs ai
      mOut     = fmap mkType . chooseType $ outputs ai
      urlPart  = (if isAccessor ai then const "" else id) $
                 (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + encodeURIComponent(" ++ i ++ ") + '/") mIdent
      mIdent   = (if isAccessor ai then const Nothing else id) $ fmap (jsId . cleanName . description) $ ident ai
  in function fParams $ ret $
        proc (ns ++ "." ++ "ajaxCall")
          [ string (method ai)
          , code $ (if (https ai) then "this.secureContextUrl" else "this.contextUrl") ++ " + '" ++ urlPart ++ "'"
          , code "params"
          , code "success"
          , code "error"
          , string $ maybe "text/plain" snd3 mInp
          , string $ maybe "text" fst3 mOut
          , maybe (code "undefined") (\(p, _, f) -> f (code p)) mInp
          , code "callOpts"
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

mkType :: DataDescription -> (String, String, Code -> Code)
mkType ds =
  case dataType ds of
    JSON  -> ("json", "text/json", call "JSON.stringify")
    XML   -> ("xml" , "text/xml", id)
    File  -> ("file", "application/octet-stream", id)
    Other -> ("text", "text/plain", id)
