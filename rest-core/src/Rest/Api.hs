{-# LANGUAGE
    GADTs
  , KindSignatures
  #-}
-- | This module allows you to combine 'Resource's into an 'Api'. This
-- can then be served using 'rest-happstack' or 'rest-snap', or used
-- to generate clients or documentation using 'rest-gen'.
module Rest.Api
  ( -- * Api data types.
    Api
  , Router (..)
  , Some1 (..)

    -- * Defining routes.
  , route
  , compose
  , (     -/)
  , (    --/)
  , (   ---/)
  , (  ----/)
  , ( -----/)
  , (------/)
  , root

    -- * Api versioning.
  , Version (..)
  , mkVersion
  , latest
  , parseVersion
  , lookupVersion
  , lookupVersion'
  , withVersion
  ) where

import Control.Applicative (Applicative)
import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split
import Data.Ord (comparing)

import Rest.Resource
import Rest.Schema (named, singleton)


import Safe

-------------------------------------------------------------------------------
-- A routing table of REST resources.

-- | An existential where the second argument has kind @(* -> *)@.

data Some1 f where Some1 :: f (a :: * -> *) -> Some1 f

-- | A 'Router' is a 'Resource' and a list of subresources.

data Router m s where
  Embed :: Resource m s sid mid aid -> [Some1 (Router s)] -> Router m s

-- | Convenience constructor constructing a route without any
-- subresource.

route :: Monad s => Resource m s sid mid aid -> Router m s
route = flip Embed []

-- | Add the second router as a subresource to the first.

compose :: Router m s -> Router s t -> Router m s
compose (Embed r xs) b = Embed r (xs ++ [Some1 b])

infixl 4 -/
infixl 5 --/
infixl 6 ---/
infixl 7 ----/
infixl 8 -----/
infixl 9 ------/

-- | Operators with the right fixities to allow you to define routes
-- without using parentheses. Start with the shortest near the root.

(-/), (--/), (---/), (----/), (-----/), (------/) :: Router m s -> Router s t -> Router m s

(     -/) = compose
(    --/) = compose
(   ---/) = compose
(  ----/) = compose
( -----/) = compose
(------/) = compose

-- | An empty router to use as the root for your API.

root :: (Applicative m, Monad m) => Router m m
root = route $ mkResourceId { schema = singleton () $ named [] }

-------------------------------------------------------------------------------

-- | An API version has three parts. The first is two are used for API
-- breaking changes, the last for non-API breaking changes.

data Version = Version
  { full  :: Int
  , major :: Int
  , minor :: Maybe Int
  } deriving (Eq, Ord)

-- | Smart constructor for 'Version'.

mkVersion :: Int -> Int -> Int -> Version
mkVersion f m l = Version f m (Just l)

instance Show Version where
  show v = show (full v) ++ "." ++ show (major v) ++ maybe "" (\x -> "." ++ show x) (minor v)

-- | An API is a list of versioned routers.

type Api m = [(Version, Some1 (Router m))]

-- | Get the latest version of an API.

latest :: Api m -> Maybe (Version, Some1 (Router m))
latest = headMay . reverse . sortBy (compare `on` fst)

-- | Parse a 'String' as a 'Version'. The string should contain two or
-- three numbers separated by dots, e.g. @1.12.3@.

parseVersion :: String -> Maybe Version
parseVersion s =
  case map readMay . splitOn "." . filter (\c -> isDigit c || c == '.') $ s of
    [ Just a, Just b, Just c ] -> Just (Version a b (Just c))
    [ Just a, Just b         ] -> Just (Version a b Nothing)
    _                          -> Nothing

-- | Look up a version in an API. The string can either be a valid
-- version according to 'parseVersion', or "latest".

lookupVersion :: String -> Api m -> Maybe (Some1 (Router m))
lookupVersion "latest" = fmap snd . latest
lookupVersion str      = (parseVersion str >>=) . flip lookupVersion'

-- | Look up a version in the API.

lookupVersion' :: Version -> Api m -> Maybe (Some1 (Router m))
lookupVersion' v versions = best (filter (matches v . fst) versions)
  where best = fmap snd . headMay . sortBy (flip (comparing fst))
        matches :: Version -> Version -> Bool
        matches (Version a b c) (Version x y z)
          | a == x && b == y && c <= z = True
          | otherwise                  = False

-- | Given a version string, an API and a fallback, do the following:
--
-- * Parse the version number or "latest".
--
-- * Look up this version.
--
-- * If ok, run the given function on it.
--
-- * If not parsed or found, return the fallback.

withVersion :: String -> Api m -> r -> (Version -> Some1 (Router m) -> r) -> r
withVersion ver api err ok =
  maybe err (uncurry ok) $
    case ver of
      "latest" -> latest api
      _        -> do pv <- parseVersion ver
                     r <- lookupVersion' pv api
                     return (pv, r)
