{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Rest.Driver.Types where

import Rest.Handler (Handler)

type Run m n = forall a. m a -> n a

data RunnableHandler n = forall m. RunnableHandler
  (Run m n)   -- ^ Runner to the base monad.
  (Handler m) -- ^ Actual handler to run.

data Method = GET | PUT | POST | DELETE | Unknown String
  deriving (Show, Eq)
