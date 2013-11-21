{-# LANGUAGE
    TemplateHaskell
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , TypeSynonymInstances
  , RankNTypes
  , KindSignatures
  , GADTs
  , NamedFieldPuns
  #-}
module Rest.Resource where

import Control.Applicative (Applicative)
import Control.Monad.Reader

import Rest.Handler
import Rest.Schema (Schema (..), Step (..))

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a }

data Resource m s sid mid aid where
  Resource :: (Applicative m, Monad m, Applicative s, Monad s) =>
    { name        :: String                      -- ^ The name for this resource, used as a path segment in routing.
    , description :: String                      -- ^ A description of the resource, used for documentation.
    , schema      :: Schema sid mid aid          -- ^ The schema for routing and identification.
    , private     :: Bool                        -- ^ Private resources are not documented, but they are exposed.
    , enter       :: forall b. sid -> s b -> m b -- ^ How to run a subresource given an id.

    , list        :: mid -> ListHandler m        -- ^ List handler, both toplevel and deeper (search).

    , statics     :: aid -> Handler m            -- ^ Static actions, e.g. signin.

    , get         :: Maybe (Handler s)           -- ^ Get a single resource identified by id.
    , update      :: Maybe (Handler s)           -- ^ Update a single resource identified by id.
    , remove      :: Maybe (Handler s)           -- ^ Delete a single resource identified by id.
    , create      :: Maybe (Handler m)           -- ^ Create a single resource, generating a new id.
    , actions     :: [(String, Handler s)]       -- ^ Actions performed on a single resource.
    , selects     :: [(String, Handler s)]       -- ^ Properties of a single resource.
    } -> Resource m s sid mid aid

mkResource :: (Applicative m, Monad m, Applicative s, Monad s) => (forall b. sid -> s b -> m b) -> Resource m s sid Void Void
mkResource e = Resource
  { name           = ""
  , description    = ""
  , schema         = Schema Nothing (Named [])
  , private        = False
  , enter          = e

  , list           = magic

  , statics        = magic

  , get            = Nothing
  , update         = Nothing
  , remove         = Nothing
  , create         = Nothing
  , actions        = []
  , selects        = []
  }

mkResourceId :: (Applicative m, Monad m) => Resource m m sid Void Void
mkResourceId = mkResource (const id)

mkResourceReaderWith :: (Applicative m, Monad m, Applicative s, Monad s) => (forall b. s b -> ReaderT sid m b) -> Resource m s sid Void Void
mkResourceReaderWith f = mkResource (\a -> flip runReaderT a . f)

mkResourceReader :: (Applicative m, Monad m) => Resource m (ReaderT sid m) sid Void Void
mkResourceReader = mkResourceReaderWith id

