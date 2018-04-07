{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Rest.Driver.Types
  ( Run
  , RunnableHandler (..)
  , Config (..)
  , mapHandler

  , module Rest.Types.Method
  ) where

import Prelude.Compat

import Control.Monad.Trans.Except
import Network.Multipart (BodyPart)

import Rest.Api (Router)
import Rest.Error (Reason_)
import Rest.Handler (Handler)
import Rest.Types.Container.Resource (Resource)
import Rest.Types.Method (Method (..))


type Run m n = forall a. m a -> n a

data RunnableHandler n = forall m. RunnableHandler
  (Run m n)   -- Runner to the base monad.
  (Handler m) -- Actual handler to run.

mapHandler :: Run m n -> RunnableHandler m -> RunnableHandler n
mapHandler run (RunnableHandler run' h) = RunnableHandler (run . run') h

newtype Config m
  = Config
  { runMultiResources :: forall s. Config m -> Router m s -> [Resource] -> ExceptT Reason_ m [BodyPart]
  }
