{-# LANGUAGE
    TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  , FlexibleInstances
  , ScopedTypeVariables
  , DeriveDataTypeable
  , GADTs
  #-}
module Rest.Types.Container
  ( List(..)
  , Key(..)
  , Map(..)
  ) where

import Control.Arrow
import Data.JSON.Schema hiding (key)
import Data.JSON.Schema.Combinators (field)
import Data.Typeable
import Generics.Regular (deriveAll, PF)
import Generics.Regular.JSON
import Generics.Regular.XmlPickler (gxpickle)
import Text.JSON
import Text.XML.HXT.Arrow.Pickle

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: a
  } deriving (Show, Typeable)

$(deriveAll ''List "PFList")
type instance PF (List a) = PFList a

instance XmlPickler a => XmlPickler (List a) where
  xpickle = gxpickle `const` (offset, count, items)

instance JSON a => JSON (List a) where
 showJSON = gshowJSON
 readJSON = greadJSON

instance JSONSchema a => JSONSchema (List a)  where
  schema = gSchema

instance Json a => Json (List a)

-------------------------------------------------------------------------------

newtype Key = Key { key :: String } deriving (Show, Typeable)

$(deriveAll ''Key "PFKey")
type instance PF Key = PFKey

instance XmlPickler Key where
  xpickle = xpElem "key" (xpWrap (Key, key) xpText0)

instance JSON Key where
  showJSON = gshowJSON
  readJSON = greadJSON

instance JSONSchema Key where
  schema = gSchema

-------------------------------------------------------------------------------

newtype Map a b = Map { unMap :: [(a, b)] } deriving (Show, Typeable)

$(deriveAll ''Map "PFMap")
type instance PF (Map a b) = PFMap a b

instance (XmlPickler a, XmlPickler b) => XmlPickler (Map a b) where
  xpickle = xpElem "map" (xpWrap (Map, unMap) (xpList (xpPair xpickle xpickle)))

instance JSON b => JSON (Map Key b) where
  showJSON = showJSON . toJSObject . map (first key) . unMap
  readJSON = fmap (Map . map (first Key) . fromJSObject) . readJSON

instance JSONSchema b => JSONSchema (Map Key b) where
  schema _ = field "key" False (schema (Proxy :: Proxy b))

instance Json b => Json (Map Key b)
