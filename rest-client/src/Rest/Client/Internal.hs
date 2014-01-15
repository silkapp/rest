{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances
           , TypeSynonymInstances
           , OverlappingInstances
           , UndecidableInstances
           , GeneralizedNewtypeDeriving
           , FlexibleContexts
           , MultiParamTypeClasses
           , TypeFamilies
           , OverloadedStrings
           #-}
module Rest.Client.Internal
 ( module Control.Monad
 , BS.ByteString
 , Network.URI.Encode.encode
 , hAccept
 , hContentType
 , Data.List.intercalate
 , Api
 , ApiT(..)
 , ApiStateC(..)
 , MonadIO(..)
 , runT
 , run
 , withHeaders
 , ApiResponse
 , ApiRequest(..)
 , ApiState(..)
 , ApiInfo(..)
 , doRequest
 , parseResult
 , responseToMaybe
 , fromXML
 , fromJSON
 , fromJSONlist
 , fromXMLlist
 , toXML
 , toJSON
 , ShowUrl (..)
 ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Cont   hiding (mapM)

import qualified Data.ByteString.Char8 as CH
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as LB

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

import Rest.Types.Container
import Rest.Types.Error
import Rest.Types.ShowUrl

import Rest.Client.Base

data ApiRequest = ApiRequest
  { method         :: String
  , uri            :: String
  , params         :: [(String, String)]
  , requestHeaders :: RequestHeaders
  , requestBody    :: BS.ByteString
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
                , HTTP.requestBody = RequestBodyBS bd
                , checkStatus = (\_ _ _ -> Nothing)
                , redirectCount = 0
                , responseTimeout = defaultTimeout
                , cookieJar = Just jar
                }
     res <- httpLbs req mn
     putApiState (ApiState (jar `mappend` responseCookieJar res))
     return res

parseResult :: (CH.ByteString -> Reason e) -> (CH.ByteString -> a) -> Response LB.ByteString -> ApiResponse e a
parseResult e c res = convertResponse
  (case T.statusCode (HTTP.responseStatus res) of
    200 -> fmap (Right . c . BS.concat . LB.toChunks) res
    _   -> fmap (Left . e . BS.concat . LB.toChunks) res
  )

fromJSON :: FromJSON a => BS.ByteString -> a
fromJSON v = (fromMaybe err . decode . LB.fromStrict) v
  where
    err = error ("Error parsing json in  api binding, this should not happen: " ++ BS.toString v)

toJSON :: ToJSON a => a -> BS.ByteString
toJSON = LB.toStrict . encode

class XmlStringToType a where
  fromXML :: BS.ByteString -> a
  toXML :: a -> BS.ByteString

instance XmlStringToType String where
  fromXML = BS.toString
  toXML = BS.fromString

instance XmlPickler a => XmlStringToType a where
  fromXML v = ( either (error ("Error parsing XML in  api binding, this should not happen: " ++ BS.toString v)) id
              . P.eitherFromXML
              . BS.toString
              ) v
  toXML = BS.fromString . P.toXML

fromJSONlist :: FromJSON a => BS.ByteString -> [a]
fromJSONlist v = ( items
                 . fromMaybe err
                 . decode
                 . LB.fromStrict
                 ) v
  where
    err = error ("Error parsing json list in  api bindings, this should not happen: " ++ BS.toString v)

fromXMLlist :: XmlPickler a => BS.ByteString -> [a]
fromXMLlist v = ( items
                . either err id
                . P.eitherFromXML
                . BS.toString
                ) v
  where
    err = error ("Error parsing xml list in  api bindings, this should not happen: " ++ BS.toString v)
