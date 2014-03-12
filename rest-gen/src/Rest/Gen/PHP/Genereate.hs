{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Rest.Gen.PHP.Generate (mkPhpApi) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Code.Build
import Code.Build.Ruby

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import Prelude hiding (catch)

import Rest.Gen.Base
import Rest.Gen.Utils
import Rest.Action hiding (ident)
import Rest.Resource

mkPhpApi :: String -> Bool -> Api m -> Maybe String
mkPhpApi "latest" priv = fmap (\(v, Some1 r) -> mkRbCode (tail $ show v) priv r) . latest
mkPhpApi ver      priv = fmap (\(Some1 r) -> mkPhpCode ver priv r) . lookupVersion ("v" ++ ver)

mkPhpCode :: String -> Bool -> Router m s -> String
mkPhpCode ver priv = showCode . mkPhp ver . sortTree . (if priv then id else noPrivate) . apiSubtrees

mkPhp :: String -> ApiResource -> Code
mkPhp ver node =
  php $ mkStack
        [ "$silkversion" .=. string ver
        , apiConstructor ver node
        , foldTreeChildren mkStack (\i ls -> mkStack $ mkRes i : ls) node
        ]

apiConstructor :: String -> ApiResource -> Code
apiConstructor ver node =
  phpClass "BaseApi" $
    [ protected "url" noCode
    , protected "version" $ string ver
    , protected "api" noCode
    , function "initialize" ["resUrl"] $ mkStack $
        [ "url" .=. ("resUrl + '/v" ++ ver ++ "/'")
        , "api" .=. "this"
        ]
    , mkStack . map resGetter . subResources $ node
    ]

mkRes :: ApiResource -> Code
mkRes node = mkResAcc node <-> mkResObj node

mkResAcc :: ApiResource -> Code
mkResAcc node =
  phpClass (accessorName $ resId node)
    [ accInitializer
    , mkPreFuncs node
    , mkStack . map resGetter . filter (not . hasAccessor) . subResources $ node
    ]

mkResObj :: ApiResource -> Code
mkResObj node =
  phpClass (className $ resId node)
    [ objInitializer
    , if hasAccessor node then get else noCode
    , mkPostFuncs node
    , mkStack . map resGetter . subResources $ node
    ]

mkPostFuncs :: ApiResource -> Code
mkPostFuncs node = mkStack . map mkFunction . filter (postAction . itemInfo) . resItems $ node

mkPreFuncs :: ApiResource -> Code
mkPreFuncs node =
  let (acs, funcs) = partition (isAccessor . itemInfo) . filter ((\i -> not $ postAction i) . itemInfo) $ resItems node
  in mkStack (map mkAccessor acs) <-> mkStack (map mkFunction funcs)

mkAccessor :: ApiAction -> Code
mkAccessor node@(ApiAction rid _ ai) =
  let params   = maybeToList mIdent
      urlPart  = (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + " ++ i ++ " + '/") mIdent
      datType  = maybe ":data" ((':':) . fst3 . mkType) $ chooseType $ outputs ai
      mIdent   = fmap (phpName . cleanName) $ ident ai
  in function (phpName $ mkFuncParts node) params $ ret $
        new (className rid) ["@url + '" ++ urlPart ++ "'", "@api", datType]

mkFunction :: ApiAction -> Code
mkFunction node@(ApiAction _ _ ai) =
  let params   = maybeToList mIdent
              ++ maybeToList (fmap fst3 mInp)
              ++ ["params = {}", "headers = {}"]
      mInp     = fmap mkType . chooseType $ inputs ai
      mOut     = fmap mkType . chooseType $ outputs ai
      urlPart  = (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + " ++ i ++ " + '/") mIdent
      mIdent   = fmap (phpName . cleanName) $ ident ai
  in function (phpName $ mkFuncParts node) params $
        call ("internalSilkRequest")
          [ code "@api"
          , code $ ':' : map toLower (show $ method ai)
          , code $ "@url + '" ++ urlPart ++ "'"
          , code "params"
          , string $ maybe "text/plain" snd3 mInp
          , code $ maybe ":data" ((':':) . fst3) mOut
          , maybe (code "nil") (\(p, _, f) -> f (code p)) mInp
          , code "headers"
          ]

accInitializer :: Code
accInitializer =
  function "initialize" ["resUrl", "myApi"]
     [ "@url"      .=. "resUrl"
     , "@api"      .=. "myApi"
     ]

objInitializer :: Code
objInitializer =
  function "initialize" ["resUrl", "myApi", "retData = :data"]
     [ "@url"      .=. "resUrl"
     , "@dataType" .=. "retData"
     , "@api"      .=. "myApi"
     ]

resGetter :: ApiResource -> Code
resGetter node =
  function (className [resName node]) [] $
    ret $ new (accessorName $ resId node) ["@url + '" ++ resName node ++ "/'", "@api"]

get :: Code
get =
  function "get" ["params = {}", "headers = {}"] $
    call "internalSilkRequest" ["@api", ":get", "@url", "params", "'text/plain'", "@dataType", "headers"]

phpName :: [String] -> String
phpName []       = ""
phpName (x : xs) = x ++ concatMap upFirst xs

className :: ResourceId -> String
className = concatMap upFirst . concatMap cleanName

accessorName :: ResourceId -> String
accessorName = concatMap upFirst . ("Access":) . concatMap cleanName

mkType :: DataDescription -> (String, String, Code -> Code)
mkType ds =
  case dataType ds of
    JSON  -> ("json", "text/json", call "mkJson")
    XML   -> ("xml" , "text/xml", (<+> ".to_s"))
    File  -> ("file", "application/octet-stream", id)
    Other -> ("data", "text/plain", id)
