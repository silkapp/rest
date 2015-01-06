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

import Rest.Api (Api)
import Rest.Driver.Perform (Rest (..))
import Rest.Driver.Types (Run)

import qualified Rest.Run          as Rest
import qualified Rest.Driver.Types as Rest

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

toRestMethod :: Happstack.Method -> Maybe Rest.Method
toRestMethod Happstack.GET    = Just Rest.GET
toRestMethod Happstack.POST   = Just Rest.POST
toRestMethod Happstack.PUT    = Just Rest.PUT
toRestMethod Happstack.DELETE = Just Rest.DELETE
toRestMethod _                = Nothing
