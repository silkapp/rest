{-# LANGUAGE
    NoImplicitPrelude
  , RankNTypes
  #-}
module Rest.Run
  ( apiToHandler
  , apiToHandler'
  , apiToHandlerWith
  , Config (..)
  ) where

import Prelude.Compat

import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Rest.Api (Api)
import Rest.Dictionary (Dicts (..))
import Rest.Driver.Perform
import Rest.Driver.Types
import qualified Rest.Driver.Routing.Internal as I

apiToHandler :: (Applicative m, Monad m) => Rest m => Api m -> m UTF8.ByteString
apiToHandler = apiToHandler' id

apiToHandler' :: (Applicative m, Monad m) => Rest n => Run m n -> Api m -> n UTF8.ByteString
apiToHandler' run = apiToHandlerWith I.defaultConfig run

apiToHandlerWith :: Config m -> Rest n => Run m n -> Api m -> n UTF8.ByteString
apiToHandlerWith cfg run api = do
  method <- getMethod
  paths  <- getPaths
  case I.routeWith cfg method paths api of
    Left  e -> failureWriter None e
    Right h -> writeResponse (mapHandler run h)
