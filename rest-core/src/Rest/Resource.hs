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

import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split
import Data.Ord (comparing)

import Rest.Action

import Safe

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

data Step sid mid aid = Named   [(String, Either aid (Cardinality (Getter sid) (Getter mid)))]
                      | Unnamed (Cardinality (String -> Maybe sid) (String -> Maybe mid))

-- | Specifies if we're identifying a single resource, or many (a
-- listing).
data Cardinality s m = Single s
                     | Many   m

-- | A 'Getter' can either be a 'Singleton' (there is only one) or it
-- can be identified 'By' a 'String'.

data Getter id = Singleton id | By (String -> Maybe id)

-- * A set of combinators for creating schemas.

withListing :: mid -> Step sid mid aid -> Schema sid mid aid
withListing mid = Schema (Just (Many mid))

noListing :: Step sid mid aid -> Schema sid mid aid
noListing = Schema Nothing

singleton :: sid -> Step sid mid aid -> Schema sid mid aid
singleton sid = Schema (Just (Single sid))

named :: [(String, Either aid (Cardinality (Getter sid) (Getter mid)))] -> Step sid mid aid
named = Named

-- TODO: name clash with action from Rest.Action

action :: aid -> Either aid (Cardinality (Getter sid) (Getter mid))
action = Left

single :: sid -> Either aid (Cardinality (Getter sid) (Getter mid))
single = Right . Single . Singleton

singleBy :: (String -> sid) -> Either aid (Cardinality (Getter sid) (Getter mid))
singleBy by = singleMaybe (Just . by)

singleRead :: Read a => (a -> sid) -> Either aid (Cardinality (Getter sid) (Getter mid))
singleRead by = singleMaybe (fmap by . readMay)

singleMaybe :: (String -> Maybe sid) -> Either aid (Cardinality (Getter sid) (Getter mid))
singleMaybe = Right . Single . By

-- TODO: name clash with listing from Resource, maybe rename back to
-- many?

listing :: mid -> Either aid (Cardinality (Getter sid) (Getter mid))
listing = Right . Many . Singleton

listingBy :: (String -> mid) -> Either aid (Cardinality (Getter sid) (Getter mid))
listingBy by = listingMaybe (Just . by)

listingRead :: Read a => (a -> mid) -> Either aid (Cardinality (Getter sid) (Getter mid))
listingRead by = listingMaybe (fmap by . readMay)

listingMaybe :: (String -> Maybe mid) -> Either aid (Cardinality (Getter sid) (Getter mid))
listingMaybe = Right . Many . By

unnamedSingle :: (String -> sid) -> Step sid mid aid
unnamedSingle by = unnamedSingleMaybe (Just . by)

unnamedSingleRead :: Read a => (a -> sid) -> Step sid mid aid
unnamedSingleRead by = unnamedSingleMaybe (fmap by . readMay)

unnamedSingleMaybe :: (String -> Maybe sid) -> Step sid mid aid
unnamedSingleMaybe = Unnamed . Single

unnamedListing :: (String -> mid) -> Step sid mid aid
unnamedListing by = unnamedListingMaybe (Just . by)

unnamedListingRead :: Read a => (a -> mid) -> Step sid mid aid
unnamedListingRead by = unnamedListingMaybe (fmap by . readMay)

unnamedListingMaybe :: (String -> Maybe mid) -> Step sid mid aid
unnamedListingMaybe = Unnamed . Many

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a }

voidHandler :: Monad m => GenHandler Void m f
voidHandler = mkHandler id (\(Env void _ _ _) -> magic void)

data Resource m s sid mid aid where
  Resource ::
    { name           :: String -- ^ The name for this resource, used as a path segment in routing.
    , description    :: String -- ^ A description of the resource, used for documentation.
    , schema         :: Schema sid mid aid -- ^ The schema for routing and identification.
    , private        :: Bool -- ^ Private resources are not documented, but they are exposed.
    , enter          :: forall b. sid -> s b -> m b -- ^ How to run a subresource given an id.

    , list           :: ListHandler mid m -- ^ List handler, both toplevel and deeper (search).

    , statics        :: Handler aid m -- ^ Static actions, e.g. signin.

    , get            :: Maybe (Handler sid m) -- ^ Get a single resource identified by id.
    , update         :: Maybe (Handler sid m) -- ^ Update a single resource identified by id.
    , remove         :: Maybe (Handler sid m) -- ^ Delete a single resource identified by id.
    , create         :: Maybe (Handler ()  m) -- ^ Create a single resource, generating a new id.
    , actions        :: [(String, Handler sid m)] -- ^ Actions performed on a single resource.
    , selects        :: [(String, Handler sid m)] -- ^ Properties of a single resource.
    } -> Resource m s sid mid aid

mkResource :: Monad m => Resource m s sid Void Void
mkResource = Resource
  { name           = ""
  , description    = ""
  , schema         = Schema Nothing (Named [])
  , private        = False
  , enter          = error "'enter' not defined for this resource"

  , list           = voidHandler

  , statics        = voidHandler

  , get            = Nothing
  , update         = Nothing
  , remove         = Nothing
  , create         = Nothing
  , actions        = []
  , selects        = []
  }

-------------------------------------------------------------------------------
-- A routing table of REST resources.

-- Generic existential types.

data Some  f where Some  :: f (a :: *     ) -> Some  f
data Some1 f where Some1 :: f (a :: * -> *) -> Some1 f

data Router m s where
  Embed :: Resource m s sid mid aid -> [Some1 (Router s)] -> Router m s

route :: Monad s => Resource m s sid mid aid -> Router m s
route = flip Embed []

compose :: Router m s -> Router s t -> Router m s
compose (Embed r xs) b = Embed r (xs ++ [Some1 b])

infixl 4 ----/
infixl 5 ---/
infixl 6 --/
infixl 7 -/

(----/), (---/), (--/), (-/) :: Router m s -> Router s t -> Router m s

(----/) = compose
( ---/) = compose
(  --/) = compose
(   -/) = compose

-------------------------------------------------------------------------------

data Version = Version
  { full  :: Int
  , major :: Int
  , minor :: Maybe Int
  } deriving (Eq, Ord)

mkVersion :: Int -> Int -> Int -> Version
mkVersion f m l = Version f m (Just l)

instance Show Version where
  show v = show (full v) ++ "." ++ show (major v) ++ maybe "" (\x -> "." ++ show x) (minor v)

type Api m = [(Version, Some1 (Router m))]

latest :: Api m -> Maybe (Version, Some1 (Router m))
latest = headMay . reverse . sortBy (compare `on` fst)

parseVersion :: String -> Maybe Version
parseVersion s =
  case map readMay . splitOn "." . filter (\c -> isDigit c || c == '.') $ s of
    [ Just a, Just b, Just c ] -> Just (Version a b (Just c))
    [ Just a, Just b         ] -> Just (Version a b Nothing)
    _                          -> Nothing

lookupVersion :: String -> Api m -> Maybe (Some1 (Router m))
lookupVersion "latest" = fmap snd . latest
lookupVersion str      = (parseVersion str >>=) . flip lookupVersion'

lookupVersion' :: Version -> Api m -> Maybe (Some1 (Router m))
lookupVersion' v versions = best (filter (matches v . fst) versions)
  where best = fmap snd . headMay . sortBy (flip (comparing fst))
        matches :: Version -> Version -> Bool
        matches (Version a b c) (Version x y z)
          | a == x && b == y && c <= z = True
          | otherwise                  = False

withVersion :: String -> Api m -> r -> (Version -> Some1 (Router m) -> r) -> r
withVersion ver api err ok =
  maybe err (uncurry ok) $
    case ver of
      "latest" -> latest api
      _        -> do pv <- parseVersion ver
                     r <- lookupVersion' pv api
                     return (pv, r)
