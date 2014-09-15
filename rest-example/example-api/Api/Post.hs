{-# LANGUAGE DeriveDataTypeable #-}
module Api.Post
  ( Identifier (..)
  , WithPost
  , resource
  , postFromIdentifier
  ) where

import Control.Applicative
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar)
import Control.Monad (unless)
import Control.Monad.Error (ErrorT, throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (lift, liftIO)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Time
import Data.Typeable
import Safe
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import Rest
import Rest.Info
import Rest.ShowUrl
import qualified Rest.Resource as R

import ApiTypes
import Type.CreatePost (CreatePost)
import Type.Post (Post (Post))
import Type.PostError (PostError (..))
import Type.User (User)
import Type.UserPost (UserPost (UserPost))
import qualified Type.CreatePost as CreatePost
import qualified Type.Post       as Post
import qualified Type.User       as User

data Identifier
  = Latest
  | ById Int
  deriving (Eq, Show, Read, Typeable)

instance Info Identifier where
  describe _ = "identifier"

instance ShowUrl Identifier where
  showUrl Latest = "latest"
  showUrl (ById i) = show i

-- | Post extends the root of the API with a reader containing the ways to identify a Post in our URLs.
-- Currently only by the title of the post.
type WithPost = ReaderT Identifier BlogApi

-- | Defines the /post api end-point.
resource :: Resource BlogApi WithPost Identifier () Void
resource = mkResourceReader
  { R.name   = "post" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("id", singleRead ById), ("latest", single Latest)]
  , R.list   = const list -- list is requested by GET /post which gives a listing of posts.
  , R.create = Just create -- PUT /post to create a new Post.
  , R.get    = Just get
  , R.remove = Just remove
  }

postFromIdentifier :: Identifier -> TVar (Set Post) -> STM (Maybe Post)
postFromIdentifier i pv = finder <$> readTVar pv
  where
    finder = case i of
      ById ident -> F.find ((== ident) . Post.id) . Set.toList
      Latest     -> headMay . sortBy (flip $ comparing Post.createdTime) . Set.toList

get :: Handler WithPost
get = mkIdHandler xmlJsonO $ \_ i -> do
  mpost <- liftIO . atomically . postFromIdentifier i =<< (lift . lift) (asks posts)
  case mpost of
    Nothing -> throwError NotFound
    Just a  -> return a

-- | List Posts with the most recent posts first.
list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  psts <- liftIO . atomically . readTVar =<< asks posts
  return . take (count r) . drop (offset r) . sortBy (flip $ comparing Post.createdTime) . Set.toList $ psts

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJson) $ \(UserPost usr pst) -> do
  -- Make sure the credentials are valid
  checkLogin usr
  pstsVar <- asks posts
  psts <- liftIO . atomically . readTVar $ pstsVar
  post <- liftIO $ toPost (Set.size psts + 1) usr pst
  -- Validate and save the post in the same transaction.
  merr <- liftIO . atomically $ do
    let vt = validTitle pst psts
    if not vt
      then return . Just $ domainReason InvalidTitle
      else if not (validContent pst)
        then return . Just $ domainReason InvalidContent
        else modifyTVar pstsVar (Set.insert post) >> return Nothing
  maybe (return post) throwError merr

remove :: Handler WithPost
remove = mkIdHandler id $ \_ i -> do
  pstsVar <- lift . lift $ asks posts
  merr <- liftIO . atomically $ do
    mpost <- postFromIdentifier i pstsVar
    case mpost of
      Nothing -> return . Just $ NotFound
      Just post -> modifyTVar pstsVar (Set.delete post) >> return Nothing
  maybe (return ()) throwError merr

-- | Convert a User and CreatePost into a Post that can be saved.
toPost :: Int -> User -> CreatePost -> IO Post
toPost i u p = do
  t <- getCurrentTime
  return Post
    { Post.id          = i
    , Post.author      = User.name u
    , Post.createdTime = t
    , Post.title       = CreatePost.title p
    , Post.content     = CreatePost.content p
    }

-- | A Post's title must be unique and non-empty.
validTitle :: CreatePost -> Set Post -> Bool
validTitle p psts =
  let pt        = CreatePost.title p
      nonEmpty  = (>= 1) . T.length $ pt
      available = F.all ((pt /=) . Post.title) psts
  in available && nonEmpty

-- | A Post's content must be non-empty.
validContent :: CreatePost -> Bool
validContent = (>= 1) . T.length . CreatePost.content

-- | Throw an error if the user isn't logged in.
checkLogin :: User -> ErrorT (Reason e) BlogApi ()
checkLogin usr = do
  usrs <- liftIO . atomically . readTVar =<< asks users
  unless (usr `F.elem` usrs) $ throwError NotAllowed
