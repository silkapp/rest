{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.PostError where

import Prelude.Compat

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data PostError = InvalidTitle | InvalidContent
  deriving (Eq, Generic, Ord, Show, Typeable)

instance FromJSON   PostError where parseJSON = gparseJson
instance JSONSchema PostError where schema    = gSchema
instance ToJSON     PostError where toJSON    = gtoJson
instance XmlPickler PostError where xpickle   = gxpickle

instance ToResponseCode PostError where
  toResponseCode _ = 400
