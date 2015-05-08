{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  , UndecidableInstances
  #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TVar)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import Database.Persist.Postgresql

import Type.Comment (Comment)
import Type.Post (Post)
import Type.User (User)
import qualified Type.Post as Post

data ServerData = ServerData
  { users    :: TVar (Set User)
  , posts    :: TVar (Set Post)
  , comments :: TVar (HashMap Post.Id (Set Comment))
  , pgPool   :: ConnectionPool
  }

newtype BlogApiT m a = BlogApi { unBlogApi :: ReaderT ServerData m a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           , MonadTrans
           )

type BlogApi = BlogApiT (SqlPersistT (NoLoggingT (ResourceT IO)))

runBlogApi :: ServerData -> BlogApi a -> IO a
runBlogApi serverdata = flip runSqlPersistMPool (pgPool serverdata) . flip runReaderT serverdata . unBlogApi
