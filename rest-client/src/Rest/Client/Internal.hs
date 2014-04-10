{-# LANGUAGE
    CPP
  , FlexibleInstances
  , OverlappingInstances
  , UndecidableInstances
  #-}
module Rest.Client.Internal
  ( module Control.Monad
  , module Data.String
  , module Data.String.ToString
  , MonadIO (..)
  , L.ByteString
  , intercalate
  , URI.encode

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
import Control.Monad.Cont
import Data.Aeson.Utils (FromJSON, ToJSON, decodeV, encode)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.String.ToString
import Network.HTTP.Conduit hiding (method, responseBody, responseHeaders)
import Network.HTTP.Types hiding (statusCode, statusMessage)
import Text.XML.HXT.Arrow.Pickle
#if MIN_VERSION_http_conduit(2,0,0)
import Data.Default (def)
#endif

import qualified Data.ByteString.Char8     as CH
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Network.HTTP.Conduit      as HTTP
import qualified Network.HTTP.Types        as HTTP

import Rest.Types.Error
import Rest.Types.ShowUrl

import qualified Network.URI.Encode as URI
import qualified Text.Xml.Pickle    as P

import Rest.Client.Base

data ApiRequest = ApiRequest
  { method         :: String
  , uri            :: String
  , params         :: [(String, String)]
  , requestHeaders :: RequestHeaders
  , requestBody    :: L.ByteString
  }

convertResponse :: Response (Either (Reason e) a) -> ApiResponse e a
convertResponse r =
  ApiResponse
   { statusCode      = HTTP.statusCode (responseStatus r)
   , statusMessage   = HTTP.statusMessage (responseStatus r)
   , httpVersion     = (\v -> (httpMajor v, httpMinor v)) (responseVersion r)
   , responseHeaders = HTTP.responseHeaders r
   , responseBody    = HTTP.responseBody r
   }

defaultTimeout :: Maybe Int
defaultTimeout = Just (1000 * 1000 * 60 * 5)

splitHost :: String -> (String, String)
splitHost hst = break (== '/') hst

doRequest :: (ApiStateC m, MonadIO m) => ApiRequest -> m (Response L.ByteString)
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

parseResult :: (L.ByteString -> Reason e) -> (L.ByteString -> a) -> Response L.ByteString -> ApiResponse e a
parseResult e c res = convertResponse $
  case HTTP.statusCode (HTTP.responseStatus res) of
    200 -> fmap (Right . c) res
    _   -> fmap (Left . e) res

fromJSON :: FromJSON a => L.ByteString -> a
fromJSON v = (fromMaybe err . decodeV) v
  where err = error ("Error parsing JSON in api binding, this should not happen: " ++ L.toString v)

toJSON :: ToJSON a => a -> L.ByteString
toJSON = encode

class XmlStringToType a where
  fromXML :: L.ByteString -> a
  toXML :: a -> L.ByteString

instance XmlStringToType String where
  fromXML = L.toString
  toXML = L.fromString

instance XmlPickler a => XmlStringToType a where
  fromXML v = ( either err id
              . P.eitherFromXML
              . L.toString
              ) v
    where err = error ("Error parsing XML in api binding, this should not happen: " ++ L.toString v)
  toXML = L.fromString . P.toXML

