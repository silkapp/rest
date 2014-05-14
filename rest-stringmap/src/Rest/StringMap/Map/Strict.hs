{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , OverlappingInstances
  , ScopedTypeVariables
  #-}
module Rest.StringMap.Map.Strict
  ( StringMap
  , fromMap
  , toMap
  , toList
  , fromList
  ) where

import Data.Aeson
import Data.JSON.Schema
import Data.Map.Strict (Map)
import Data.String
import Data.String.ToString
import Data.Typeable
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Map.Strict as M

import Rest.StringMap.Util

newtype StringMap a b = StringMap { unM :: Map a b }
  deriving (Eq, Show, Typeable)

fromMap :: Map a b -> StringMap a b
fromMap = StringMap

toMap :: StringMap a b -> Map a b
toMap = unM

fromList :: Ord a => [(a,b)] -> StringMap a b
fromList = StringMap . M.fromList

toList :: StringMap a b -> [(a, b)]
toList = M.toList . unM

mapKeys :: Ord l => (k -> l) -> StringMap k v -> StringMap l v
mapKeys f = StringMap . M.mapKeys f . unM

instance XmlPickler b => XmlPickler (StringMap String b) where
  xpickle = pickleStringMap fromList toList

instance (Ord a, IsString a, ToString a, XmlPickler b) => XmlPickler (StringMap a b) where
  xpickle = pickleMap mapKeys mapKeys

instance (ToString a, ToJSON b) => ToJSON (StringMap a b) where
  toJSON = toJSON . M.mapKeys toString . unM

instance (Ord a, IsString a, FromJSON b) => FromJSON (StringMap a b) where
  parseJSON = fmap (StringMap . M.mapKeys fromString) . parseJSON

instance JSONSchema b => JSONSchema (StringMap a b) where
  schema _ = mapSchema (Proxy :: Proxy b)
