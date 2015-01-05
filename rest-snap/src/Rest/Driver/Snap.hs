{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
module Rest.Driver.Snap (apiToHandler, apiToHandler') where

import Safe
import Snap.Core
import Snap.Util.FileServe (defaultMimeTypes)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8  as UTF8
import qualified Data.CaseInsensitive  as CI
import qualified Data.HashMap.Strict   as M
import qualified Network.URI.Encode    as URI
import qualified Snap.Core             as Snap

import Rest.Api (Api)
import Rest.Driver.Perform (Rest (..))
import Rest.Driver.Types (Run)

import qualified Rest.Run          as Rest
import qualified Rest.Driver.Types as Rest

apiToHandler :: Api Snap -> Snap ()
apiToHandler = apiToHandler' id

apiToHandler' :: Run m Snap -> Api m -> Snap ()
apiToHandler' run api = Rest.apiToHandler' run api >>= writeLBS

instance Rest Snap where
  getHeader nm       = getsRequest (fmap UTF8.toString . Snap.getHeader (CI.mk . UTF8.fromString $ nm))
  getParameter  nm   = getsRequest (fmap UTF8.toString . (>>= headMay) . rqParam (UTF8.fromString nm))
  getBody            = readRequestBody (1 * 1024 * 1024)
  getMethod          = getsRequest (toRestMethod . rqMethod)
  getPaths           = getsRequest (map (UTF8.toString . URI.decodeByteString) . filter (not . Char8.null) . Char8.split '/' . rqPathInfo)
  lookupMimeType     = return . fmap UTF8.toString . flip M.lookup defaultMimeTypes
  setHeader nm v     = modifyResponse (Snap.setHeader (CI.mk . UTF8.fromString $ nm) (UTF8.fromString v))
  setResponseCode cd = modifyResponse (Snap.setResponseCode cd)

toRestMethod :: Snap.Method -> Maybe Rest.Method
toRestMethod Snap.GET    = Just Rest.GET
toRestMethod Snap.POST   = Just Rest.POST
toRestMethod Snap.PUT    = Just Rest.PUT
toRestMethod Snap.DELETE = Just Rest.DELETE
toRestMethod _           = Nothing
