{-# LANGUAGE ExistentialQuantification #-}
-- | This module contains data types and combinators for defining a
-- 'Schema' for your 'Resource'. A 'Schema' has three type parameters,
-- specifying the identifiers for a single resource, a listing, and a
-- top-level (static) action. After routing, these identifiers will be
-- passed to the 'Handler'.
module Rest.Schema
  ( -- * A set of combinators for creating schemas.

    -- ** Top level
    withListing
  , noListing
  , singleton
    -- ** Named endpoints
  , named
  , static
  , single
  , singleBy
  , singleRead
  , listing
  , listingBy
  , listingRead
    -- ** Unnamed endpoints
  , unnamedSingle
  , unnamedSingleRead
  , unnamedListing
  , unnamedListingRead

    -- * The core schema data types.

  , Schema (..)
  , Step (..)
  , Cardinality (..)
  , Getter (..)
  , Id (..)
  , Endpoint
  ) where

import Rest.Dictionary (Ident (..))
import Rest.Info (Info)

-- * A set of combinators for creating schemas.

-- ** Top level

-- | A schema with a top level listing.

withListing :: mid -> Step sid mid aid -> Schema sid mid aid
withListing mid = Schema (Just (Many mid))

-- | A schema with no top level listing.

noListing :: Step sid mid aid -> Schema sid mid aid
noListing = Schema Nothing

-- | A schema with a singleton at the top level.

singleton :: sid -> Step sid mid aid -> Schema sid mid aid
singleton sid = Schema (Just (Single sid))

-- ** Named endpoints

-- | A list of named endpoints.

named :: [(String, Endpoint sid mid aid)] -> Step sid mid aid
named = Named

-- | A top level action endpoint for this resource.

static :: aid -> Endpoint sid mid aid
static = Left

-- | A singleton resource endpoint.

single :: sid -> Endpoint sid mid aid
single = Right . Single . Singleton

-- | A single resource endpoint with a string identifier.

singleBy :: (String -> sid) -> Endpoint sid mid aid
singleBy = singleIdent StringId

-- | A single resource endpoint with an identifier that can be read.

singleRead :: (Read a, Info a) => (a -> sid) -> Endpoint sid mid aid
singleRead = singleIdent ReadId

-- | A single resource identified as specified by the 'Ident'.

singleIdent :: Ident a -> (a -> sid) -> Endpoint sid mid aid
singleIdent ident = Right . Single . By . Id ident

-- | A listing endpoint.

listing :: mid -> Endpoint sid mid aid
listing = Right . Many . Singleton

-- | A listing endpoint with a string identifier.

listingBy :: (String -> mid) -> Endpoint sid mid aid
listingBy = listingIdent StringId

-- | A listing with an identifier that can be read.

listingRead :: (Read a, Info a) => (a -> mid) -> Endpoint sid mid aid
listingRead = listingIdent ReadId

-- | A listing identified as specified by the 'Ident'.
listingIdent :: Ident a -> (a -> mid) -> Endpoint sid mid aid
listingIdent ident = Right . Many . By . Id ident

-- ** Unnamed endpoints

-- | An unnamed single resource with a string identifier.

unnamedSingle :: (String -> sid) -> Step sid mid aid
unnamedSingle = unnamedSingleIdent StringId

-- | An unnamed single resource with an identifier that can be read.

unnamedSingleRead :: (Read a, Info a) => (a -> sid) -> Step sid mid aid
unnamedSingleRead = unnamedSingleIdent ReadId

-- | An unnamed single resource identified as specified by the
-- 'Ident'.

unnamedSingleIdent :: Ident a -> (a -> sid) -> Step sid mid aid
unnamedSingleIdent ident = Unnamed . Single . Id ident

-- | An unnamed listing with a string identifier.

unnamedListing :: (String -> mid) -> Step sid mid aid
unnamedListing = unnamedListingIdent StringId

-- | An unnamed listing with an identifier that can be read.
unnamedListingRead :: (Read a, Info a) => (a -> mid) -> Step sid mid aid
unnamedListingRead = unnamedListingIdent ReadId

-- | An unnamed listing identified as specified by the 'Ident'.

unnamedListingIdent :: Ident a -> (a -> mid) -> Step sid mid aid
unnamedListingIdent ident = Unnamed . Many . Id ident

-- * The core schema data types.

-- | A 'Schema' describes how (part of the) route to a resource looks,
-- and returns an identifier for a single resource ('sid'), many
-- resources ('mid') or a static action ('aid').
-- The first argument specifies the top level resource (no path
-- segments). The second specifies a what happens at the first step in
-- the path.

data Schema sid mid aid = Schema (Maybe (Cardinality sid mid)) (Step sid mid aid)

-- | A step in the routing of a resource. A part of the uri either
-- identifies a 'Named' resource, or an 'Unnamed' resource. Named
-- resources can be static actions ('Left') or one or many singletons or
-- by's.

data Step sid mid aid = Named   [(String, Endpoint sid mid aid)]
                      | Unnamed (Cardinality (Id sid) (Id mid))

-- | Specifies if we're identifying a single resource, or many (a
-- listing).
data Cardinality s m = Single s
                     | Many   m

-- | A 'Getter' can either be a 'Singleton' (there is only one) or it
-- can be identified 'By' an 'Id'entifier.

data Getter id = Singleton id | By (Id id)

-- | An identification of an item in a resource. It contains a
-- dictionary describing how to identify the resource, and a function
-- for this identification type to an @id@.

data Id id = forall a. Id (Ident a) (a -> id)

-- | A named endpoint: an static action, a single item of many items.

type Endpoint sid mid aid = Either aid (Cardinality (Getter sid) (Getter mid))
