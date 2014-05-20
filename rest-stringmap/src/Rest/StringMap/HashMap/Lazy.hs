{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , OverlappingInstances
  , ScopedTypeVariables
  #-}
module Rest.StringMap.HashMap.Lazy
  ( StringHashMap
  , fromHashMap
  , toHashMap
  , fromList
  , toList
  ) where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.JSON.Schema
import Data.String
import Data.String.ToString
import Data.Typeable
import Text.XML.HXT.Arrow.Pickle
import qualified Data.HashMap.Lazy as H

import Rest.StringMap.Util

newtype StringHashMap a b = StringHashMap { unH :: HashMap a b }
  deriving (Eq, Show, Typeable)

fromHashMap :: HashMap a b -> StringHashMap a b
fromHashMap = StringHashMap

toHashMap :: StringHashMap a b -> HashMap a b
toHashMap = unH

fromList :: (Eq a, Hashable a) => [(a, b)] -> StringHashMap a b
fromList = StringHashMap . H.fromList

toList :: StringHashMap a b -> [(a, b)]
toList = H.toList . unH

-- | This is used with the assumption that a = b => f a = f b
mapKeys ::  (Hashable l, Eq l) => (k -> l) -> StringHashMap k v -> StringHashMap l v
mapKeys f = StringHashMap . H.foldlWithKey' (\h k v -> H.insert (f k) v h) H.empty . unH

-- | Base case since XmlPickler expects strings
instance XmlPickler b => XmlPickler (StringHashMap String b) where
  xpickle = pickleStringMap fromList toList

-- | General case
instance (Eq a, Hashable a, IsString a, ToString a, XmlPickler b) => XmlPickler (StringHashMap a b) where
  xpickle = pickleMap mapKeys mapKeys

instance (ToString a, ToJSON b) => ToJSON (StringHashMap a b) where
  toJSON = toJSON . toHashMap . mapKeys toString

instance (Eq a, Hashable a, IsString a, FromJSON b) => FromJSON (StringHashMap a b) where
  parseJSON = fmap (mapKeys fromString . fromHashMap) . parseJSON

instance JSONSchema b => JSONSchema (StringHashMap a b) where
  schema _ = mapSchema (Proxy :: Proxy b)

