{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , TupleSections
  , UndecidableInstances
  #-}
module Rest.Types.Container
  ( List(..)
  , SomeOutput(..)
  ) where

import Prelude hiding (mapM)

import Data.Aeson.Types
import Data.JSON.Schema (JSONSchema (..), gSchema)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml
import qualified Data.JSON.Schema as JSONSchema

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: [a]
  } deriving (Generic, Eq, Ord, Show, Read, Typeable, Functor, Foldable, Traversable)

instance XmlPickler a => XmlPickler (List a) where xpickle   = gxpickle
instance ToJSON     a => ToJSON     (List a) where toJSON    = gtoJson
instance FromJSON   a => FromJSON   (List a) where parseJSON = gparseJson
instance JSONSchema a => JSONSchema (List a) where schema    = gSchema

-------------------------------------------------------------------------------

data SomeOutput where
  SomeOutput :: (XmlPickler o, ToJSON o, JSONSchema o) => o -> SomeOutput

deriving instance Typeable SomeOutput

instance XmlPickler SomeOutput where
  xpickle = PU
    (\(SomeOutput e) st -> appPickle xpickle e st)
    (throwMsg "Cannot unpickle SomeOutput.")
    Any

instance ToJSON SomeOutput where
  toJSON (SomeOutput r) = toJSON r

instance JSONSchema SomeOutput where
  schema _ = JSONSchema.Any
