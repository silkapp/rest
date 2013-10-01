{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module Rest.Driver.Perform where

import Control.Monad.Error

import Rest.Handler
import Rest.Dictionary
import Rest.Error

import Rest.Driver.Types

data Writable m = forall h p j o e. Writable (Dict h p j o e) (ErrorT (Reason e) m o)

-- TODO: secure flag is unused.
runAction :: HasInput m => RunnableHandler m -> Writable m
runAction (RunnableHandler run (GenHandler dict act _)) = Writable dict $ do
  inputs <- fetchInputs dict
  mapErrorT run (act inputs)

class Monad m => HasInput m where
  fetchInputs :: Dict h p j o e -> ErrorT (Reason e) m (Env h p j)

writeResponse :: CanOutput m => Writable m -> m (Response m)
writeResponse (Writable (_, _, _, o, e) act) = do
  res <- runErrorT $ do
    output <- act
    writeOutput o output
  case res of
    Left  er -> writeFailure e er
    Right r  -> return r

class Monad m => CanOutput m where
  type Response m :: *
  writeOutput  :: Outputs o -> o        -> ErrorT (Reason e) m (Response m)
  writeFailure :: Errors  e -> Reason e ->                   m (Response m)
