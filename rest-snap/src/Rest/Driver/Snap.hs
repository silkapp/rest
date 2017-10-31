{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    CPP
  , GeneralizedNewtypeDeriving
  , NoImplicitPrelude
  , RankNTypes
  #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module Rest.Driver.Snap
  ( apiToHandler
  , apiToHandler'
  ) where

import Prelude.Compat

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

import qualified Rest.Driver.Types as Rest
import qualified Rest.Run          as Rest

newtype Snapped m a = Snapped { unSnapped :: m a }
  deriving (Applicative, Functor, Monad)

apiToHandler :: (MonadSnap m, Rest m, Applicative m, Monad m) => Api m -> m ()
apiToHandler = apiToHandler' id

apiToHandler' :: (Applicative m, Monad m, MonadSnap n) => Run m n -> Api m -> n ()
apiToHandler' run api = writeLBS =<< unSnapped (Rest.apiToHandler' (Snapped . run) api)

instance (MonadSnap m) => Rest (Snapped m) where
  getHeader nm       = Snapped $ getsRequest (fmap UTF8.toString . Snap.getHeader (CI.mk . UTF8.fromString $ nm))
  getParameter  nm   = Snapped $ getsRequest (fmap UTF8.toString . (>>= headMay) . rqParam (UTF8.fromString nm))
  getBody            = Snapped $ readRequestBody (1 * 1024 * 1024)
  getMethod          = Snapped $ getsRequest (toRestMethod . rqMethod)
  getPaths           = Snapped $ getsRequest (map (UTF8.toString . URI.decodeByteString) . filter (not . Char8.null) . Char8.split '/' . rqPathInfo)
  lookupMimeType     = Snapped . return . fmap UTF8.toString . flip M.lookup defaultMimeTypes
  setHeader nm v     = Snapped $ modifyResponse (Snap.setHeader (CI.mk . UTF8.fromString $ nm) (UTF8.fromString v))
  setResponseCode cd = Snapped $ modifyResponse (Snap.setResponseCode cd)

toRestMethod :: Snap.Method -> Maybe Rest.Method
toRestMethod Snap.GET    = Just Rest.GET
toRestMethod Snap.POST   = Just Rest.POST
toRestMethod Snap.PUT    = Just Rest.PUT
toRestMethod Snap.DELETE = Just Rest.DELETE
toRestMethod _           = Nothing
