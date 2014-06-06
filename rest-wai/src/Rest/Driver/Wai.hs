{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, RankNTypes #-}
module Rest.Driver.Wai (apiToApplication) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.Trans (lift)
import Network.HTTP.Types.Status (status200)
import Network.Mime (defaultMimeMap)
import Network.Wai

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.CaseInsensitive  as CI
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Map              as Map
import qualified Data.Text             as Text

import Rest.Api (Api)
import Rest.Driver.Types (Run)
import Rest.Driver.RestM (RestInput(..), RestOutput(..), runRestM)

import qualified Rest.Run          as Rest
import qualified Rest.Driver.Types as Rest

apiToApplication :: Run m IO -> Api m -> Application
apiToApplication run api req =
  do ri <- toRestInput req
     (bs, ro) <- runRestM ri (Rest.apiToHandler' (lift . run) api)
     return (fromRestOutput ro bs)

toRestInput :: Request -> IO RestInput
toRestInput req =
  do bs <- lazyRequestBody req
     return $ RestInput
       { headers    = HashMap.fromList
                    . map (string . CI.original *** string)
                    . requestHeaders
                    $ req

       , parameters = HashMap.fromList
                    . map (string *** maybe "" string)
                    . queryString
                    $ req

       , body       = bs

       , method     = case requestMethod req of
                        "GET"    -> Rest.GET
                        "POST"   -> Rest.POST
                        "PUT"    -> Rest.PUT
                        "DELETE" -> Rest.DELETE
                        other    -> Rest.Unknown (string other)

       , paths      = text <$> pathInfo req

       , mimeTypes  = HashMap.fromList
                    . fmap (text *** string)
                    $ Map.toList defaultMimeMap
       }
     where string = Char8.unpack
           text   = Text.unpack

fromRestOutput :: RestOutput -> Lazy.ByteString -> Response
fromRestOutput (RestOutput hs rc) bs =
  responseLBS (maybe status200 toEnum rc)
              ((CI.mk . Char8.pack *** Char8.pack) <$> HashMap.toList hs)
              bs

