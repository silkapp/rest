{-# LANGUAGE GADTs
           , KindSignatures
           #-}
module Rest.Api where

import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split
import Data.Ord (comparing)

import Rest.Resource
import Rest.Schema (singleton, named)


import Safe

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
root = route $ mkResourceId { schema = singleton () $ named [] }

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
