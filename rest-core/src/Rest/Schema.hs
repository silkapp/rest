{-# LANGUAGE ExistentialQuantification #-}
module Rest.Schema where

import Rest.Dictionary (Ident (..))
import Rest.Info (Info)

-- | A 'Schema' described how (part of the) route to a resource looks,
-- and returns an identifier for a single resource ('sid'), many
-- resources ('mid') or an action ('aid').
-- The first argument specifies the top level resource (no path
-- segments). The second specifies a what happens at the first step in
-- the path.

data Schema sid mid aid = Schema (Maybe (Cardinality sid mid)) (Step sid mid aid)

-- | A step in the routing of a resource. A part of the uri either
-- identifies a 'Named' resource, or an 'Unnamed' resource. Named
-- resources can be actions ('Left') or one or many singletons or
-- by's.

data Step sid mid aid = Named   [(String, Endpoint sid mid aid)]
                      | Unnamed (Cardinality (Id sid) (Id mid))

-- | Specifies if we're identifying a single resource, or many (a
-- listing).
data Cardinality s m = Single s
                     | Many   m

-- | A 'Getter' can either be a 'Singleton' (there is only one) or it
-- can be identified 'By' a 'String'.

data Getter id = Singleton id | By (Id id)

-- | An identification of an item in a resource. It contains a
-- dictionary describing how to identify the resource, and a function
-- for this identification type to an 'id'.

data Id id = forall a. Id (Ident a) (a -> id)

-- | A named endpoint: an action, a single item of many items.

type Endpoint sid mid aid = Either aid (Cardinality (Getter sid) (Getter mid))

-- * A set of combinators for creating schemas.

withListing :: mid -> Step sid mid aid -> Schema sid mid aid
withListing mid = Schema (Just (Many mid))

noListing :: Step sid mid aid -> Schema sid mid aid
noListing = Schema Nothing

singleton :: sid -> Step sid mid aid -> Schema sid mid aid
singleton sid = Schema (Just (Single sid))

named :: [(String, Endpoint sid mid aid)] -> Step sid mid aid
named = Named

action :: aid -> Endpoint sid mid aid
action = Left

single :: sid -> Endpoint sid mid aid
single = Right . Single . Singleton

singleBy :: (String -> sid) -> Endpoint sid mid aid
singleBy = singleIdent StringId

singleRead :: (Show a, Read a, Info a) => (a -> sid) -> Endpoint sid mid aid
singleRead = singleIdent ReadId

singleIdent :: Ident a -> (a -> sid) -> Endpoint sid mid aid
singleIdent ident = Right . Single . By . Id ident

-- TODO: name clash with listing from Resource, maybe rename back to
-- many?

listing :: mid -> Endpoint sid mid aid
listing = Right . Many . Singleton

listingBy :: (String -> mid) -> Endpoint sid mid aid
listingBy = listingIdent StringId

listingRead :: (Show a, Read a, Info a) => (a -> mid) -> Endpoint sid mid aid
listingRead = listingIdent ReadId

listingIdent :: Ident a -> (a -> mid) -> Endpoint sid mid aid
listingIdent ident = Right . Many . By . Id ident

unnamedSingle :: (String -> sid) -> Step sid mid aid
unnamedSingle = unnamedSingleIdent StringId

unnamedSingleRead :: (Show a, Read a, Info a) => (a -> sid) -> Step sid mid aid
unnamedSingleRead = unnamedSingleIdent ReadId

unnamedSingleIdent :: Ident a -> (a -> sid) -> Step sid mid aid
unnamedSingleIdent ident = Unnamed . Single . Id ident

unnamedListing :: (String -> mid) -> Step sid mid aid
unnamedListing = unnamedListingIdent StringId

unnamedListingRead :: (Show a, Read a, Info a) => (a -> mid) -> Step sid mid aid
unnamedListingRead = unnamedListingIdent ReadId

unnamedListingIdent :: Ident a -> (a -> mid) -> Step sid mid aid
unnamedListingIdent ident = Unnamed . Many . Id ident
