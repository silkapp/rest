{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Rest.Driver.Types where

import Rest.Action (GenHandler)

type Run m n = forall a. m a -> n a

data RunnableHandler n = forall m id f. RunnableHandler
  id             -- ^ Identifier (path segment).
  (Run m n)      -- ^ Runner to the base monad.
  (GenHandler id m f) -- ^ Actual handler to run.
