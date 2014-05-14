module Rest.StringMap.Util
  ( pickleStringMap
  , pickleMap
  , mapSchema
  , mapToJSON
  , mapParseJSON
  ) where

import Data.Aeson.Types
import Data.JSON.Schema (JSONSchema, Schema, schema)
import Data.JSON.Schema.Combinators (field)
import Data.Proxy (Proxy)
import Data.String (IsString (..))
import Data.String.ToString (ToString (..))
import Text.XML.HXT.Arrow.Pickle (PU, XmlPickler, xpElem, xpList, xpPair, xpTextAttr, xpWrap, xpickle)

pickleStringMap :: XmlPickler b => ([(String, b)] -> m) -> (m -> [(String, b)]) -> PU m
pickleStringMap fromList toList =
  xpElem "map"
    $ xpWrap (fromList, toList)
    $ xpList (xpElem "value" (xpPair (xpTextAttr "key") xpickle))

pickleMap :: (XmlPickler m, ToString k, IsString k) => ((String -> k) -> m -> m') -> ((k -> String) -> m' -> m) -> PU m'
pickleMap mapKeys mapKeys' = xpWrap (mapKeys fromString, mapKeys' toString) xpickle

mapSchema :: JSONSchema a => Proxy a -> Schema
mapSchema = field "key" False . schema

mapToJSON :: (ToString a, ToJSON m) => ((a -> String) -> m' -> m) -> m' -> Value
mapToJSON mapKeys = toJSON . mapKeys toString

mapParseJSON :: (FromJSON m, IsString k) => ((String -> k) -> m -> m') -> Value -> Parser m'
mapParseJSON mapKeys = fmap (mapKeys fromString) . parseJSON
