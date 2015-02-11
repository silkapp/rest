{-# LANGUAGE ScopedTypeVariables #-}
module Api.Post.Comment (resource) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Error
import Data.List
import Data.Monoid
import Data.Ord
import Data.Time
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as Set

import Rest
import qualified Rest.Resource as R

import Api.Post (WithPost, postFromIdentifier)
import ApiTypes
import Type.Comment (Comment (Comment))
import Type.UserComment (UserComment (UserComment))
import qualified Type.Comment as Comment
import qualified Type.Post    as Post
import qualified Type.User    as User

type Identifier = String

type WithComment = ReaderT Identifier WithPost

resource :: Resource WithPost WithComment Identifier () Void
resource = mkResourceReader
  { R.name   = "comment"
  , R.schema = withListing () $ named [("id", singleRead id)]
  , R.list   = const list
  , R.create = Just create -- PUT /post to create a new Post.
  }

list :: ListHandler WithPost
list = mkListing xmlJsonO handler
  where
    handler :: Range -> ExceptT Reason_ WithPost [Comment]
    handler r = do
      postId <- getPostId `orThrow` NotFound
      comms <- liftIO . atomically . readTVar
           =<< (lift . lift) (asks comments)
      return . take (count r) . drop (offset r)
             . sortBy (flip $ comparing Comment.createdTime)
             . maybe [] Set.toList . H.lookup postId $ comms

create :: Handler WithPost
create = mkInputHandler xmlJson handler
  where
    handler :: UserComment -> ExceptT Reason_ WithPost Comment
    handler ucomm = do
      postId <- getPostId `orThrow` NotFound
      comm   <- liftIO $ userCommentToComment ucomm
      comms  <- lift . lift $ asks comments
      liftIO . atomically $
        modifyTVar' comms (H.insertWith (<>) postId (Set.singleton comm))
      return comm

getPostId :: ExceptT Reason_ WithPost (Maybe Post.Id)
getPostId = do
  postIdent <- ask
  return . fmap Post.id
        =<< liftIO . atomically . postFromIdentifier postIdent
        =<< (lift . lift) (asks posts)

userCommentToComment :: UserComment -> IO Comment
userCommentToComment (UserComment u content) = do
  t <- getCurrentTime
  return $ Comment (User.name u) t content
