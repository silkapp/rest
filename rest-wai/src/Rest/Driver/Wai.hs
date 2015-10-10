{-# LANGUAGE
    CPP
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , RankNTypes
  #-}
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
import Rest.Driver.RestM (RestInput (..), RestOutput (..), runRestM)
import Rest.Driver.Types (Run)
import qualified Rest.Driver.Types as Rest
import qualified Rest.Run          as Rest

apiToApplication :: (Applicative m, Monad m) => Run m IO -> Api m -> Application
apiToApplication run api req =
#if MIN_VERSION_wai(3,0,0)
  \cont ->
  do ri <- toRestInput req
     (bs, ro) <- runRestM ri (Rest.apiToHandler' (lift . run) api)
     cont (fromRestOutput ro bs)
#else
  do ri <- toRestInput req
     (bs, ro) <- runRestM ri (Rest.apiToHandler' (lift . run) api)
     return $ fromRestOutput ro bs
#endif

toRestInput :: Request -> IO RestInput
toRestInput req =
  do bs <- lazyRequestBody req
     return $ RestInput
       { headers    = HashMap.fromList
                    . map (CI.mk . string . CI.original *** string)
                    . requestHeaders
                    $ req

       , parameters = HashMap.fromList
                    . map (string *** maybe "" string)
                    . queryString
                    $ req

       , body       = bs

       , method     = case requestMethod req of
                        "GET"    -> Just Rest.GET
                        "POST"   -> Just Rest.POST
                        "PUT"    -> Just Rest.PUT
                        "DELETE" -> Just Rest.DELETE
                        _        -> Nothing

       , paths      = text <$> filter (not . Text.null) (pathInfo req)

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
