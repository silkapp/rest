module Rest.StringMap.Util
  ( pickleStringMap
  , pickleMap
  , mapSchema
  ) where

import Data.JSON.Schema (JSONSchema, Schema (Map), schema)
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
mapSchema = Map . schema
