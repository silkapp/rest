{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.PostError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data PostError = InvalidTitle | InvalidContent
  deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler PostError where xpickle = gxpickle
instance JSONSchema PostError where schema = gSchema
instance FromJSON   PostError
instance ToJSON     PostError

instance ToResponseCode PostError where
  toResponseCode _ = 400
