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

import Rest.Handler
import Rest.Schema (Schema (..), Step (..), singleton, named)

import Safe

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a }

data Resource m s sid mid aid where
  Resource :: (Monad m, Monad s) =>
    { name           :: String                      -- ^ The name for this resource, used as a path segment in routing.
    , description    :: String                      -- ^ A description of the resource, used for documentation.
    , schema         :: Schema sid mid aid          -- ^ The schema for routing and identification.
    , private        :: Bool                        -- ^ Private resources are not documented, but they are exposed.
    , enter          :: forall b. sid -> s b -> m b -- ^ How to run a subresource given an id.

    , list           :: mid -> ListHandler m        -- ^ List handler, both toplevel and deeper (search).

    , statics        :: aid -> Handler m            -- ^ Static actions, e.g. signin.

    , get            :: Maybe (Handler s)           -- ^ Get a single resource identified by id.
    , update         :: Maybe (Handler s)           -- ^ Update a single resource identified by id.
    , remove         :: Maybe (Handler s)           -- ^ Delete a single resource identified by id.
    , create         :: Maybe (Handler m)           -- ^ Create a single resource, generating a new id.
    , actions        :: [(String, Handler s)]       -- ^ Actions performed on a single resource.
    , selects        :: [(String, Handler s)]       -- ^ Properties of a single resource.
    } -> Resource m s sid mid aid

mkResource :: (Monad m, Monad s) => Resource m s sid Void Void
mkResource = Resource
  { name           = ""
  , description    = ""
  , schema         = Schema Nothing (Named [])
  , private        = False
  , enter          = error "'enter' not defined for this resource"

  , list           = magic

  , statics        = magic

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

root :: Monad m => Router m m
root = route $ mkResource { enter = const id, schema = singleton () $ named [] }

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
