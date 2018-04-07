{-# LANGUAGE ScopedTypeVariables #-}
module Rest.Gen.JavaScript (mkJsApi) where

import Prelude.Compat hiding ((.))

import Control.Category ((.))
import Data.Maybe
import Text.StringTemplate
import qualified Data.Label.Total   as L
import qualified Data.List.NonEmpty as NList

import Code.Build
import Code.Build.JavaScript
import Rest.Api (Router, Version)
import Rest.Gen.Base
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.NoAnnotation as N

mkJsApi :: N.ModuleName -> Bool -> Version -> Router m s -> IO String
mkJsApi ns priv ver r =
  do prelude <- render . setManyAttrib attrs . newSTMP <$> readContent "files/Javascript/prelude.js"
     epilogue <- render . setManyAttrib attrs . newSTMP <$> readContent "files/Javascript/epilogue.js"
     let cod = showCode $ mkStack
                [ unModuleName ns ++ ".prototype.version" .=. string (show ver)
                , mkJsCode (unModuleName ns) priv r
                ]
     return $ mkJsModule (prelude ++ cod ++ epilogue)
  where attrs = [("apinamespace", unModuleName ns), ("dollar", "$")]

mkJsModule :: String -> String
mkJsModule content = "(function (window) {\n\n" ++ content ++ "\n\n})(this);"

mkJsCode :: String -> Bool -> Router m s -> Code
mkJsCode ns priv = mkJs ns . sortTree . (if priv then id else noPrivate) . apiSubtrees

mkJs :: String -> ApiResource -> Code
mkJs ns = foldTreeChildren mkStack (\i ls -> mkStack $ mkRes ns i : ls)

mkRes :: String -> ApiResource -> Code
mkRes ns node = mkStack
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
      mIdent   = jsId . cleanName . description <$> ident ai
  in function fParams
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
      mIdent   = (if isAccessor ai then const Nothing else id) $ jsId . cleanName . description <$> ident ai
  in function fParams $ ret $
        call (ns ++ "." ++ "ajaxCall")
          [ string (method ai)
          , code $ (if https ai then "this.secureContextUrl" else "this.contextUrl") ++ " + '" ++ urlPart ++ "'"
          , code "params"
          , code "success"
          , code "error"
          , string $ maybe "text/plain" snd3 mInp
          , string mOut
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
    (x : xs) ->
      let res = x ++ concatMap upFirst xs
      in if res `elem` reservedWords
         then res ++ "_"
         else res

jsDir :: [String] -> String
jsDir = concatMap upFirst

jsId :: [String] -> String
jsId []       = ""
jsId (x : xs) =
  let res = x ++ concatMap upFirst xs
  in if res `elem` reservedWords
     then res ++ "_"
     else res

-- | Javascript reserved words in the broadest sense.
-- Taken from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords

reservedWords :: [String]
reservedWords = es6Reserved ++ futureReserved ++ futureReservedStrict ++ oldFutureReserved
             ++ reservedLiterals
  where
    es6Reserved =
      [ "break", "case", "class", "catch", "const", "continue", "debugger", "default", "delete"
      , "do", "else", "export", "extends", "finally", "for", "function", "if", "import", "in"
      , "instanceof", "let", "new", "return", "super", "switch", "this", "throw", "try", "typeof"
      , "var", "void", "while", "with", "yield"
      ]
    futureReserved = ["enum", "await"]
    futureReservedStrict =
      [ "implements", "package", "protected", "static", "interface", "private", "public" ]
    oldFutureReserved =
      [ "abstract", "boolean", "byte", "char", "double", "final", "float", "goto", "int", "long"
      , "native", "short", "synchronized", "transient", "volatile"
      ]
    reservedLiterals = ["null", "false", "true"]

mkType :: DataType -> (String, String, Code -> Code)
mkType dt =
  case dt of
    String -> ("text", "text/plain", id)
    XML    -> ("xml" , "text/xml", id)
    JSON   -> ("json", "text/json", call "JSON.stringify")
    File   -> ("file", "application/octet-stream", id)
    Other  -> ("text", "text/plain", id)
