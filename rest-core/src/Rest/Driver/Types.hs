{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Rest.Driver.Types
  ( Run
  , RunnableHandler (..)
  , mapHandler

  , module Rest.Types.Method
  ) where

import Rest.Handler (Handler)
import Rest.Types.Method (Method (..))

type Run m n = forall a. m a -> n a

data RunnableHandler n = forall m. RunnableHandler
  (Run m n)   -- Runner to the base monad.
  (Handler m) -- Actual handler to run.

mapHandler :: Run m n -> RunnableHandler m -> RunnableHandler n
mapHandler run (RunnableHandler run' h) = RunnableHandler (run . run') h
