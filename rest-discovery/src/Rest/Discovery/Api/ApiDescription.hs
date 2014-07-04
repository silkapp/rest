{-# LANGUAGE
    OverloadedStrings
  #-}
module Rest.Discovery.Api.ApiDescription (resource) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Foldable as F
import qualified Data.Text     as T

import Rest (Handler, ListHandler, Range (count, offset),
             Resource, Void, domainReason, mkInputHandler, mkListing, mkResourceReader, named, singleRead, mkHandler,
             withListing, xmlJsonE, xmlJsonI, xmlJsonO)
import qualified Rest.Resource as R
import Rest.Schema (singleBy)

import Rest.Discovery.Type.ApiDescription (ApiDescription(..))
import Control.Monad.Reader
import Rest.Gen.Base
import Rest.Api (Router)
import Control.Applicative
import Data.Function (on)
import Data.List hiding (head, span)
import Data.Maybe

-- | Defines the /apis end-point.

resource :: (Applicative m, Monad m) => Router m m -> Resource m (ReaderT T.Text m) T.Text () Void
resource router = mkResourceReader
  { R.name   = "apis" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("name", singleBy T.pack)]
  , R.list   = const (list router) -- requested by GET /apis, gives a paginated listing of apis.
  , R.get    = Just $ get router
  }

apiDescriptionFromApiResource :: ApiResource -> ApiDescription
apiDescriptionFromApiResource =
    ApiDescription <$> ((T.pack . resName))
                   <*> (linkText . resLink)
                   <*> ((map apiDescriptionFromApiResource) . subResources)

linkText :: Link -> T.Text
linkText = T.concat . (map linkItem)
  where linkItem (LParam idf)   = T.pack ("/<" ++ idf ++ ">")
        linkItem (LAccess lnks) = T.concat $ map linkText $ reverse $ sortBy (compare `on` length) lnks
        linkItem x              = T.pack ("/" ++ itemString x)

list :: (Applicative m, Monad m) => Router m m -> ListHandler m
list router = mkListing xmlJsonO $ \r -> do
  let apiresource = apiSubtrees $ router
  let apilisting = [ apiDescriptionFromApiResource $ apiresource]
  return . take (count r) . drop (offset r) $ apilisting

get :: (Applicative m, Monad m) => Router m m -> Handler (ReaderT T.Text m)
get router = mkHandler xmlJsonO $ \_ -> do
    name <- ask
    let apiresource_tree = apiSubtrees $ router
    let apiresource = findApiResource name apiresource_tree
    return $ fmap apiDescriptionFromApiResource apiresource

findApiResource :: T.Text -> ApiResource -> Maybe ApiResource
findApiResource name tree =
    if name == (T.pack . resName $ tree)
    then Just tree
    else if null (subResources tree)
         then Nothing
         else listToMaybe . catMaybes . map (findApiResource name) $ subResources tree

