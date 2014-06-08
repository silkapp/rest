{-# LANGUAGE
    OverloadedStrings
  #-}
module Api.ApiDescription (resource) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (liftIO)
import Data.Set (Set)
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import Rest (Handler, ListHandler, Range (count, offset),
             Resource, Void, domainReason, mkInputHandler, mkListing, mkResourceReader, named, singleRead, mkHandler,
             withListing, xmlJsonE, xmlJsonI, xmlJsonO)
import qualified Rest.Resource as R
import Rest.Schema (singleBy)

import ApiTypes (BlogApi, ServerData (..))
import Type.User (User)
import Type.ApiDescription (ApiDescription(..))
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo
import Control.Monad.Reader
import Rest.Gen.Base
import Rest.Api (Router)
import Control.Applicative
import Data.Function (on)
import Data.List hiding (head, span)

-- | Defines the /apis end-point.

resource :: Router BlogApi BlogApi -> Resource BlogApi (ReaderT T.Text BlogApi) T.Text () Void
resource router = mkResourceReader
  { R.name   = "apis" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("name", singleBy T.pack)]
  , R.list   = const (list router) -- requested by GET /apis, gives a paginated listing of apis.
  , R.create = Nothing
  , R.get = Just $ get router
  }

apiDescriptionFromApiResource :: ApiResource -> ApiDescription
apiDescriptionFromApiResource =
    ApiDescription <$> ((T.pack . resName))
                   <*> (linkText . resLink)
                   <*> ((map apiDescriptionFromApiResource) . subResources)

linkText :: Link -> T.Text
linkText = (T.intercalate ", ") . (map linkItem)
  where linkItem (LParam idf)   = T.pack ("/<" ++ idf ++ ">")
        linkItem (LAccess lnks) = T.concat $ map linkText $ reverse $ sortBy (compare `on` length) lnks
        linkItem x              = T.pack ("/" ++ itemString x)

list :: Router BlogApi BlogApi -> ListHandler BlogApi
list router = mkListing xmlJsonO $ \r -> do
  let apiresource = apiSubtrees $ router
  let apilisting = [ apiDescriptionFromApiResource $ apiresource]
  return . take (count r) . drop (offset r) $ apilisting

get :: Router BlogApi BlogApi -> Handler (ReaderT T.Text BlogApi)
get router = mkHandler xmlJsonO $ \_ -> do
    name <- ask
    -- TODO: traverse the apiTree and find the ApiResource
    return $ ApiDescription name "" []

