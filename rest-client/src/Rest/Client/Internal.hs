{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
module Rest.Client.Internal
  ( module Control.Monad
  , MonadIO (..)
  , LB.ByteString
  , intercalate
  , Network.URI.Encode.encode

  , module Rest.Client.Base
  , ShowUrl (..)
  , hAccept
  , hContentType

  , ApiRequest(..)
  , doRequest
  , parseResult
  , fromJSON
  , toJSON
  , fromXML
  , toXML
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Cont   hiding (mapM)

import qualified Data.ByteString.Char8 as CH
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LB

import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

import Network.HTTP.Conduit hiding (method, responseBody, responseHeaders)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as T
import Network.HTTP.Types hiding (statusCode, statusMessage)
import qualified Network.URI.Encode

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Text.XML.HXT.Arrow.Pickle
import qualified Text.Xml.Pickle as P

import Rest.Types.Error
import Rest.Types.ShowUrl

import Rest.Client.Base

data ApiRequest = ApiRequest
  { method         :: String
  , uri            :: String
  , params         :: [(String, String)]
  , requestHeaders :: RequestHeaders
  , requestBody    :: LB.ByteString
  }

convertResponse :: Response (Either (Reason e) a) -> ApiResponse e a
convertResponse r =
  ApiResponse
   { statusCode      = T.statusCode (responseStatus r)
   , statusMessage   = T.statusMessage (responseStatus r)
   , httpVersion     = (\v -> (httpMajor v, httpMinor v)) (responseVersion r)
   , responseHeaders = HTTP.responseHeaders r
   , responseBody    = HTTP.responseBody r
   }

defaultTimeout :: Maybe Int
defaultTimeout = Just (1000 * 1000 * 60 * 5)

splitHost :: String -> (String, String)
splitHost hst = break (== '/') hst

doRequest :: (ApiStateC m, MonadIO m) => ApiRequest -> m (Response LB.ByteString)
doRequest (ApiRequest m ur ps rhds bd) =
  do mn  <- fmap manager askApiInfo
     hst <- fmap apiHost askApiInfo
     prt <- fmap apiPort askApiInfo
     hds <- fmap headers askApiInfo
     jar <- fmap cookies getApiState
     let (h, p) = splitHost hst
         req = def
                { HTTP.method = CH.pack m
                , host = CH.pack h
                , port = prt
                , path = CH.pack (p ++ "/" ++ ur)
                , queryString = (renderQuery False . simpleQueryToQuery . Prelude.map (CH.pack *** CH.pack)) ps
                , HTTP.requestHeaders = rhds ++ Prelude.map (fromString *** CH.pack) hds
                , HTTP.requestBody = RequestBodyLBS bd
                , checkStatus = (\_ _ _ -> Nothing)
                , redirectCount = 0
                , responseTimeout = defaultTimeout
                , cookieJar = Just jar
                }
     res <- httpLbs req mn
     putApiState (ApiState (jar `mappend` responseCookieJar res))
     return res

parseResult :: (LB.ByteString -> Reason e) -> (LB.ByteString -> a) -> Response LB.ByteString -> ApiResponse e a
parseResult e c res = convertResponse
  (case T.statusCode (HTTP.responseStatus res) of
    200 -> fmap (Right . c) res
    _   -> fmap (Left . e) res
  )

fromJSON :: FromJSON a => LB.ByteString -> a
fromJSON v = (fromMaybe err . decode) v
  where
    err = error ("Error parsing json in  api binding, this should not happen: " ++ LB.toString v)

toJSON :: ToJSON a => a -> LB.ByteString
toJSON = encode

class XmlStringToType a where
  fromXML :: LB.ByteString -> a
  toXML :: a -> LB.ByteString

instance XmlStringToType String where
  fromXML = LB.toString
  toXML = LB.fromString

instance XmlPickler a => XmlStringToType a where
  fromXML v = ( either (error ("Error parsing XML in  api binding, this should not happen: " ++ LB.toString v)) id
              . P.eitherFromXML
              . LB.toString
              ) v
  toXML = LB.fromString . P.toXML
