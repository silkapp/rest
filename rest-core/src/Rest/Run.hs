{-# LANGUAGE RankNTypes #-}
module Rest.Run
  ( apiToHandler
  , apiToHandler'
  ) where

import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Rest.Api (Api)
import Rest.Dictionary ( Dicts (..) )
import Rest.Driver.Perform
import Rest.Driver.Routing
import Rest.Driver.Types

apiToHandler :: Rest m => Api m -> m UTF8.ByteString
apiToHandler = apiToHandler' id

apiToHandler' :: Rest n => Run m n -> Api m -> n UTF8.ByteString
apiToHandler' run api = do
  method <- getMethod
  paths  <- getPaths
  case route method paths api of
    Left  e                        -> failureWriter None e
    Right (RunnableHandler run' h) -> writeResponse (RunnableHandler (run . run') h)
