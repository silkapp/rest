{-# LANGUAGE ScopedTypeVariables #-}
module Api.Test.DashedName (resource) where

import Control.Monad.Except
import Control.Monad.Reader
import Rest
import qualified Rest.Resource as R

import Api.Test (WithText)

type SiteId = String

type WithSiteSubscription = ReaderT SiteId WithText

resource :: Resource WithText WithSiteSubscription SiteId Void Void
resource = mkResourceReader
  { R.name   = "foo-bar"
  , R.schema = noListing $ named [("id", singleRead id)]
  , R.remove = Just remove
  }

remove :: Handler WithSiteSubscription
remove = mkConstHandler id handler
  where
    handler :: ExceptT Reason_ WithSiteSubscription ()
    handler = return ()
