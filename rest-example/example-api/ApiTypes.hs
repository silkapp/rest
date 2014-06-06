{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
import Data.Set (Set)

import Type.Post (Post)
import Type.User (User)

data ServerData = ServerData
  { users :: TVar (Set User)
  , posts :: TVar (Set Post)
  }

newtype BlogApi a = BlogApi { unBlogApi :: ReaderT ServerData IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           )

runBlogApi :: ServerData -> BlogApi a -> IO a
runBlogApi serverdata = flip runReaderT serverdata . unBlogApi
