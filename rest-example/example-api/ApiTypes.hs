{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , NoImplicitPrelude
  #-}
module ApiTypes where

import Prelude.Compat

import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)

import Type.Comment (Comment)
import Type.Post (Post)
import Type.User (User)
import qualified Type.Post as Post

data ServerData = ServerData
  { users    :: TVar (Set User)
  , posts    :: TVar (Set Post)
  , comments :: TVar (HashMap Post.Id (Set Comment))
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
