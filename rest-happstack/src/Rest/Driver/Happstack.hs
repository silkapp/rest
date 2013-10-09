{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
module Rest.Driver.Happstack (apiToHandler, apiToHandler') where

import Control.Applicative
import Control.Concurrent.MVar (readMVar)
import Control.Monad
import Control.Monad.Trans (MonadIO (liftIO))
import Happstack.Server

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map             as M
import qualified Happstack.Server     as Happstack

import Rest.Driver.Perform (Rest (..))
import Rest.Driver.Types (Run)
import Rest.Resource (Api)

import qualified Rest.Driver.Perform as Rest
import qualified Rest.Driver.Routing as Rest

apiToHandler :: (Functor m, MonadPlus m, MonadIO m) => Api (ServerPartT m) -> ServerPartT m Response
apiToHandler = apiToHandler' id

apiToHandler' :: (Functor n, MonadPlus n, MonadIO n) => Run m (ServerPartT n) -> Api m -> ServerPartT n Response
apiToHandler' run api = toResponse <$> Rest.apiToHandler' run api

instance (Functor m, MonadPlus m, MonadIO m) => Rest (ServerPartT m) where
  getHeader nm     = fmap UTF8.toString . Happstack.getHeader nm <$> askRq
  getParameter  nm = Just <$> look nm <|> pure Nothing
  getBody =
    do rq <- askRq
       bdy <- liftIO (readMVar (rqBody rq))
       return (unBody bdy)
  getMethod       = toRestMethod . rqMethod <$> askRq
  getPaths        = rqPaths <$> askRq
  lookupMimeType  = return . flip M.lookup Happstack.mimeTypes
  setHeader       = Happstack.setHeaderM
  setResponseCode = Happstack.setResponseCode

toRestMethod :: Happstack.Method -> Rest.Method
toRestMethod Happstack.GET    = Rest.GET
toRestMethod Happstack.POST   = Rest.POST
toRestMethod Happstack.PUT    = Rest.PUT
toRestMethod Happstack.DELETE = Rest.DELETE
toRestMethod mthd             = Rest.Unknown (show mthd)
