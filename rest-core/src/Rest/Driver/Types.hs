{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Rest.Driver.Types where

import Rest.Action (Handler)

type Run m n = forall a. m a -> n a

data RunnableHandler n = forall m id. RunnableHandler
  id             -- ^ Identifier (path segment).
  (Run m n)      -- ^ Runner to the base monad.
  (Handler id m) -- ^ Actual handler to run.
