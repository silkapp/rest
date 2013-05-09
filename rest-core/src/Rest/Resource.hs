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
import Data.List
import Data.List.Split
import Data.Ord (comparing)

import Rest.Action

import Safe

-------------------------------------------------------------------------------
-- A description of a single REST resource.

data Resource m s a where
  Resource ::
    { identifier     :: String
    , private        :: Bool
    , enter          :: forall b. a -> s b -> m b

    -- Specific cases for working on multiple resources at the same time.
    , multiGet       :: Maybe (Handler m [a])
    , multiGetBy     :: [(String, Handler m [a])]
    , multiActions   :: [(String, Action m)]

    -- Working with a single resource at the same time.
    , singleGet      :: [(String, Handler m a)]
    , singleGetBy    :: [(String, Handler m a)]
    , singleCreate   :: Maybe (Action m)
    , singleDelete   :: Maybe (Action s)
    , singleUpdate   :: [(String, Action m)]
    , singleUpdateBy :: [(String, Action m)]
    , singleSelects  :: [(String, Action s)]
    , singleActions  :: [(String, Action s)]

    , resourceDescription :: String
    } -> Resource m s a

mkResource :: Monad m => Resource m s a
mkResource = Resource
  { identifier     = ""
  , private        = False
  , enter          = error "'enter' not defined for this resource"

  , multiGet       = Nothing
  , multiGetBy     = []
  , multiActions   = []

  , singleCreate   = Nothing
  , singleGet      = []
  , singleGetBy    = []
  , singleDelete   = Nothing
  , singleUpdate   = []
  , singleUpdateBy = []
  , singleSelects  = []
  , singleActions  = []

  , resourceDescription = ""
  }

-- | Which names does this resource claim.

names :: Resource m s a -> [String]
names r = concatMap ($ r)
  [ map fst . multiGetBy
  , map fst . multiActions
  , map fst . singleGet
  , map fst . singleGetBy
  , map fst . singleUpdate
  , map fst . singleUpdateBy
  , map fst . singleSelects
  , map fst . singleActions
  ]

-------------------------------------------------------------------------------
-- A routing table of REST resources.

data Router m s where
  Embed :: Monad s => Resource m s a -> [Some1 (Router s)] -> Router m s

route :: Monad s => Resource m s a -> Router m s
route = flip Embed []

compose :: Router m s -> Router s t -> Router m s
compose (Embed r xs) b = Embed r (xs ++ [Some1 b])

infixl 5 ---/
infixl 6 --/
infixl 7 -/

(---/), (--/), (-/) :: Router m s -> Router s t -> Router m s

(---/) = compose
( --/) = compose
(  -/) = compose

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
