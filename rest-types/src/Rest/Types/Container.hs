{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
   DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Rest.Types.Container
  ( List(..)
  , StringMap(..)
  , fromStringMap
  , toStringMap
  ) where

import Control.Arrow
import Data.JSON.Schema hiding (key)
import Data.JSON.Schema.Combinators (field)
import Data.Map (Map)
import Data.String
import Data.String.ToString
import Data.Typeable
import Generics.Regular (deriveAll, PF)
import Generics.Regular.JSON
import Generics.Regular.XmlPickler (gxpickle)
import Text.JSON
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Map as M

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: [a]
  } deriving (Show, Typeable)

deriveAll ''List "PFList"
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

newtype StringMap a b = StringMap { unMap :: [(a, b)] } deriving (Show, Typeable)

deriveAll ''StringMap "PFStringMap"
type instance PF (StringMap a b) = PFStringMap a b

instance (IsString a, ToString a, XmlPickler b) => XmlPickler (StringMap a b) where
  xpickle = xpElem "map" (xpWrap (StringMap, unMap) (xpList (xpPair (xpElem "key" (xpWrap (fromString,toString) xpText)) xpickle)))

instance (ToString a, IsString a, JSON b) => JSON (StringMap a b) where
  showJSON = showJSON . toJSObject . map (first toString) . unMap
  readJSON = fmap (StringMap . map (first fromString) . fromJSObject) . readJSON

instance (IsString a, ToString a, JSONSchema b) => JSONSchema (StringMap a b) where
  schema _ = field "key" False (schema (Proxy :: Proxy b))

instance (IsString a, ToString a, Json b) => Json (StringMap a b)

fromStringMap :: (Ord a, IsString a, ToString a) => StringMap a b -> Map a b
fromStringMap = M.fromList . unMap

toStringMap :: (Ord a, IsString a, ToString a) => Map a b -> StringMap a b
toStringMap = StringMap . M.toList

