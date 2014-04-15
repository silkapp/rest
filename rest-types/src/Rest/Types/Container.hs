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
  , SomeOutput(..)
  ) where

import Prelude hiding (mapM)

import Data.Aeson.Types
import Data.JSON.Schema hiding (Object, Value)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular (PF, deriveAll)
import Generics.Regular.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: [a]
  } deriving (Generic, Show, Typeable)

deriveAll ''List "PFList"
type instance PF (List a) = PFList a

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

instance ToJSON SomeOutput where toJSON (SomeOutput r) = toJSON r
--  readJSON _ = Error "Cannot read SomeOutput from JSON."

instance JSONSchema SomeOutput where
  schema _ = Choice [] -- TODO: should be something like Any
