{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rest.Driver.RestM
  ( RestM
  , runRestM
  , runRestM_
  , RestInput (..)
  , emptyInput
  , RestOutput (..)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (HashMap)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.HashMap.Strict       as H

import Rest.Driver.Perform (Rest)
import qualified Rest.Driver.Perform as Rest
import qualified Rest.Driver.Types   as Rest

data RestInput = RestInput
  { headers    :: HashMap (CI String) String
  , parameters :: HashMap String String
  , body       :: UTF8.ByteString
  , method     :: Maybe Rest.Method
  , paths      :: [String]
  , mimeTypes  :: HashMap String String
  }

emptyInput :: RestInput
emptyInput = RestInput
  { headers    = H.empty
  , parameters = H.empty
  , body       = mempty
  , method     = Just Rest.GET
  , paths      = []
  , mimeTypes  = H.empty
  }

data RestOutput = RestOutput
  { headersSet   :: HashMap String String
  , responseCode :: Maybe Int
  } deriving Show

instance Monoid RestOutput where
  mempty = RestOutput { headersSet = H.empty, responseCode = Nothing }
  o1 `mappend` o2 = RestOutput
    { headersSet   = headersSet o2 `H.union` headersSet o1
    , responseCode = responseCode o2 <|> responseCode o1
    }

outputHeader :: String -> String -> RestOutput
outputHeader h v = mempty { headersSet = H.singleton h v }

outputCode :: Int -> RestOutput
outputCode cd = mempty { responseCode = Just cd }

newtype RestM m a = RestM { unRestM :: ReaderT RestInput (WriterT RestOutput m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans RestM where
  lift = RestM . lift . lift

runRestM :: RestInput -> RestM m a -> m (a, RestOutput)
runRestM i = runWriterT . flip runReaderT i . unRestM

runRestM_ :: Functor m => RestInput -> RestM m a -> m a
runRestM_ i = fmap fst . runRestM i

instance (Functor m, Applicative m, Monad m) => Rest (RestM m) where
  getHeader    h     = RestM $ asks (H.lookup (mk h) . headers   )
  getParameter p     = RestM $ asks (H.lookup p      . parameters)
  getBody            = RestM $ asks body
  getMethod          = RestM $ asks method
  getPaths           = RestM $ asks paths
  lookupMimeType t   = RestM $ asks (H.lookup t      . mimeTypes)
  setHeader h v      = RestM $ tell (outputHeader h v)
  setResponseCode cd = RestM $ tell (outputCode cd)
