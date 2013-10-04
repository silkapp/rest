{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module Rest.Driver.Perform where

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Label.Total as L

import Rest.Handler
import Rest.Dictionary
import Rest.Error

import Rest.Driver.Types

data Writable m = forall h p j o e. Writable (Dict h p j o e) (ErrorT (Reason e) m o)

runAction :: HasInput m => RunnableHandler m -> Writable m
runAction (RunnableHandler run (GenHandler dict act _)) = Writable dict $ do
  inp <- fetchInputs dict
  mapErrorT run (act inp)

class Monad m => HasInput m where
  fetchInputs :: Dict h p j o e -> ErrorT (Reason e) m (Env h p j)

writeResponse :: CanOutput m => Writable m -> m (Response m)
writeResponse (Writable dict act) = do
  res <- runErrorT $ do
    let os = L.get outputs dict
    validator os
    output <- act
    writeOutput os output
  case res of
    Left  er -> writeFailure (L.get errors dict) er
    Right r  -> return r

class Monad m => CanOutput m where
  type Response m :: *
  writeOutput  :: Outputs o -> o        -> ErrorT (Reason e) m (Response m)
  writeFailure :: Errors  e -> Reason e ->                   m (Response m)
  validator    :: Outputs o             -> ErrorT (Reason e) m ()

instance HasInput  m => HasInput  (ReaderT r m) where
  fetchInputs = mapErrorT lift . fetchInputs

instance CanOutput m => CanOutput (ReaderT r m) where
  type Response (ReaderT r m) = Response m
  writeOutput  os = mapErrorT lift . writeOutput  os
  writeFailure es =           lift . writeFailure es
  validator       = mapErrorT lift . validator
