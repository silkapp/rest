{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  , UndecidableInstances
  #-}
module Rest.Types.Container
  ( List(..)
  , StringMap(..)
  , fromStringMap
  , toStringMap
  , SomeOutput(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.JSON.Schema hiding (Object, Value)
import Data.JSON.Schema.Combinators (field)
import Data.Map (Map)
import Data.String
import Data.String.ToString
import Data.Text (pack, unpack)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular (PF, deriveAll)
import Generics.Regular.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: [a]
  } deriving (Generic, Show, Typeable)

deriveAll ''List "PFList"
type instance PF (List a) = PFList a

instance XmlPickler a => XmlPickler (List a) where xpickle = gxpickle
instance ToJSON     a => ToJSON     (List a) where toJSON    = gtoJson
instance FromJSON   a => FromJSON   (List a) where parseJSON = gparseJson
instance JSONSchema a => JSONSchema (List a) where schema    = gSchema

-------------------------------------------------------------------------------

newtype StringMap a b = StringMap { unMap :: [(a, b)] } deriving (Show, Typeable)

deriveAll ''StringMap "PFStringMap"
type instance PF (StringMap a b) = PFStringMap a b

instance (IsString a, ToString a, XmlPickler b) => XmlPickler (StringMap a b) where
  xpickle = xpElem "map" (xpWrap (StringMap, unMap) (xpList (xpPair (xpElem "key" (xpWrap (fromString,toString) xpText)) xpickle)))

instance (ToString a, ToJSON b) => ToJSON (StringMap a b) where
  toJSON = toJSON . Object . H.fromList . map (\(a,b) -> pack (toString a) .= b) . unMap

instance (IsString a, FromJSON b) => FromJSON (StringMap a b) where
  parseJSON = withObject "StringMap" (fmap StringMap . mapM (\(k,v) -> (fromString . unpack $ k,) <$> parseJSON v) . H.toList)

instance (IsString a, ToString a, JSONSchema b) => JSONSchema (StringMap a b) where
  schema _ = field "key" False (schema (Proxy :: Proxy b))

fromStringMap :: (Ord a, IsString a, ToString a) => StringMap a b -> Map a b
fromStringMap = M.fromList . unMap

toStringMap :: (Ord a, IsString a, ToString a) => Map a b -> StringMap a b
toStringMap = StringMap . M.toList

-------------------------------------------------------------------------------

data SomeOutput where
  SomeOutput :: (XmlPickler o, ToJSON o, JSONSchema o) => o -> SomeOutput

deriving instance Typeable SomeOutput

instance XmlPickler SomeOutput where
  xpickle = PU
    (\(SomeOutput e) st -> appPickle xpickle e st)
    (throwMsg "Cannot unpickle SomeOutput.")
    Any

instance ToJSON SomeOutput where toJSON (SomeOutput r) = toJSON r
--  readJSON _ = Error "Cannot read SomeOutput from JSON."

instance JSONSchema SomeOutput where
  schema _ = Choice [] -- TODO: should be something like Any
