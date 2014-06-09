{-# LANGUAGE ScopedTypeVariables #-}
module Api.Post.Comment (resource) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans (lift, liftIO)
import Data.List
import Data.Monoid
import Data.Ord
import Data.Time
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as Set

import Rest
import qualified Rest.Resource as R

import Api.Post (WithPost)
import ApiTypes
import Type.Comment (Comment (Comment))
import Type.UserComment (UserComment (UserComment))
import qualified Type.Comment as Comment
import qualified Type.User    as User


resource :: Resource WithPost WithPost Void () Void
resource = mkResourceId
  { R.name   = "comment"
  , R.schema = withListing () $ named []
  , R.list   = const list
  , R.create = Just create -- PUT /post to create a new Post.
  }

list :: ListHandler WithPost
list = mkListing xmlJsonO $ \r -> do
  post  <- ask
  comms <- liftIO . atomically . readTVar =<< (lift . lift) (asks comments)
  return . take (count r) . drop (offset r)
         . sortBy (flip $ comparing Comment.createdTime)
         . maybe [] Set.toList . H.lookup post $ comms

create :: Handler WithPost
create = mkInputHandler (xmlJson) $ \ucomm -> do
  post  <- ask
  comm  <- liftIO $ userCommentToComment ucomm
  comms <- lift . lift $ asks comments
  liftIO . atomically $
    modifyTVar' comms (H.insertWith (<>) post (Set.singleton comm))
  return comm

userCommentToComment :: UserComment -> IO Comment
userCommentToComment (UserComment u content) = do
  t <- getCurrentTime
  return $ Comment (User.name u) t content
