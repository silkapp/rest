{-# LANGUAGE ScopedTypeVariables #-}
module Rest.Gen.Ruby (mkRbApi) where

import Prelude.Compat hiding ((.))

import Control.Category ((.))
import Data.Char
import Data.List.Compat
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.ByteString.UTF8 as B
import qualified Data.Label.Total     as L
import qualified Data.List.NonEmpty   as NList

import Code.Build
import Code.Build.Ruby
import Rest.Api (Router, Version)
import Rest.Gen.Base
import Rest.Gen.Types
import Rest.Gen.Utils
import qualified Rest.Gen.NoAnnotation as N

mkRbApi :: N.ModuleName -> Bool -> Version -> Router m s -> IO String
mkRbApi ns priv ver r =
  do let prelude = replace "SilkApi" (unModuleName ns) $ B.toString rbBase
     let cod = showCode . mkRb (unModuleName ns) ver . sortTree . (if priv then id else noPrivate) . apiSubtrees $ r
     return $ cod ++ "\n" ++ prelude

replace :: String -> String -> String -> String
replace from to = intercalate to . splitOn from

mkRb :: String -> Version -> ApiResource -> Code
mkRb ns ver node =
  rbModule ns $ mkStack
        [ "@version" .=. string (show ver)
        , apiConstructor ver node
        , foldTreeChildren mkStack (\i ls -> mkStack $ mkRes i : ls) node
        ]

apiConstructor :: Version -> ApiResource -> Code
apiConstructor ver node =
  rbClass "BaseApi"
    [ function "initialize" ["resUrl"] $ mkStack
        [ "@url" .=. ("resUrl + '/v" ++ show ver ++ "/'")
        , "@api" .=. "self"
        ]
    , mkStack . map resGetter . subResources $ node
    ]

mkRes :: ApiResource -> Code
mkRes node = mkResAcc node <-> mkResObj node

mkResAcc :: ApiResource -> Code
mkResAcc node =
  rbClass (accessorName $ resId node)
    [ accInitializer
    , mkPreFuncs node
    , mkStack . map resGetter . filter (not . hasAccessor) . subResources $ node
    ]

mkResObj :: ApiResource -> Code
mkResObj node =
  rbClass (className $ resId node)
    [ objInitializer
    , if hasAccessor node then get else noCode
    , mkPostFuncs node
    , mkStack . map resGetter . subResources $ node
    ]

mkPostFuncs :: ApiResource -> Code
mkPostFuncs = mkStack . map mkFunction . filter (postAction . itemInfo) . resItems

mkPreFuncs :: ApiResource -> Code
mkPreFuncs node =
  let (acs, funcs) = partition (isAccessor . itemInfo) . filter (not . postAction . itemInfo) $ resItems node
  in mkStack (map mkAccessor acs) <-> mkStack (map mkFunction funcs)

mkAccessor :: ApiAction -> Code
mkAccessor node@(ApiAction rid _ ai) =
  let fParams  = maybeToList mIdent
      urlPart  = (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + " ++ i ++ " + '/") mIdent
      datType  = maybe ":data" ((':':) . fst3 . mkType . L.get (dataType . desc) . chooseType)
               . NList.nonEmpty . outputs $ ai
      mIdent   = rbName . cleanName . description <$> ident ai
  in function (rbName $ mkFuncParts node) fParams $ ret $
        new (className rid) ["@url + '" ++ urlPart ++ "'", "@api", datType]

mkFunction :: ApiAction -> Code
mkFunction node@(ApiAction _ _ ai) =
  let fParams   = maybeToList mIdent
              ++ maybeToList (fmap fst3 mInp)
              ++ ["params = {}", "headers = {}"]
      mInp     = fmap (mkType . L.get (dataType . desc) . chooseType) . NList.nonEmpty . inputs $ ai
      -- TODO Other clients call responseAcceptType here
      mOut     = fmap (mkType . L.get (dataType . desc) . chooseType) . NList.nonEmpty . outputs $ ai
      urlPart  = (if resDir ai == "" then "" else resDir ai ++ "/")
              ++ maybe "" (\i -> "' + " ++ i ++ " + '/") mIdent
      mIdent   = rbName . cleanName . description <$> ident ai
  in function (rbName $ mkFuncParts node) fParams $
        call "internalSilkRequest"
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

rbName :: [String] -> String
rbName []       = ""
rbName (x : xs) = downFirst x ++ concatMap upFirst xs

className :: ResourceId -> String
className = concatMap upFirst . concatMap cleanName

accessorName :: ResourceId -> String
accessorName = concatMap upFirst . ("Access":) . concatMap cleanName

mkType :: DataType -> (String, String, Code -> Code)
mkType dt =
  case dt of
    String -> ("data", "text/plain", id)
    XML    -> ("xml" , "text/xml", (<+> ".to_s"))
    JSON   -> ("json", "text/json", call "mkJson")
    File   -> ("file", "application/octet-stream", id)
    Other  -> ("data", "text/plain", id)
