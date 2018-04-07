{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Api.Test.Err2 (Err (..)) where

import Prelude.Compat

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Rest

data Err = Err deriving (Generic, Show, Typeable)
instance ToJSON     Err where toJSON    = gtoJson
instance FromJSON   Err where parseJSON = gparseJson
instance JSONSchema Err where schema    = gSchema
instance XmlPickler Err where xpickle   = gxpickle

instance ToResponseCode Err where
  toResponseCode _ = 400
