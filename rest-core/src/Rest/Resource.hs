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
-- | A 'Resource' type for representing a REST resource, as well as
-- smart constructors for empty resources which can then be filled in
-- using record updates.
module Rest.Resource where

import Control.Applicative (Applicative)
import Control.Monad.Reader

import Rest.Handler
import Rest.Schema (Schema (..), Step (..))

-- * The @Resource@ type.

-- | The 'Resource' data type represents a single resource in a REST
-- API. Handlers run in a monad @m@, while things below this resource
-- run in @s@. The identifiers @sid@, @mid@ and @aid@ identify a
-- single item, a listing and an action.

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

-- * Smart constructors for empty resources.

-- | Create an empty resource given an 'enter' function. It has no
-- name, so if you wish to route to this resource, you should set one.

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

-- | Make a resource that doesn't add any information for
-- subresources (i.e. 'enter' is set to 'id').

mkResourceId :: (Applicative m, Monad m) => Resource m m sid Void Void
mkResourceId = mkResource (const id)

-- | Make a resource that provides the single resource identifier to
-- its subresources.

mkResourceReader :: (Applicative m, Monad m) => Resource m (ReaderT sid m) sid Void Void
mkResourceReader = mkResourceReaderWith id

-- | Make a resource that provides the single resource identifier to
-- its subresources, by giving a conversion function to a 'ReaderT'.
-- If @s@ is a newtype around 'ReaderT', for example, the function
-- should unwrap the newtype.

mkResourceReaderWith :: (Applicative m, Monad m, Applicative s, Monad s) => (forall b. s b -> ReaderT sid m b) -> Resource m s sid Void Void
mkResourceReaderWith f = mkResource (\a -> flip runReaderT a . f)

-- * The @Void@ type.

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a }

