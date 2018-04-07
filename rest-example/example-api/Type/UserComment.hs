{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.UserComment where

import Prelude.Compat

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.User (User)

data UserComment = UserComment
  { user    :: User
  , comment :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler UserComment where xpickle   = gxpickle
instance JSONSchema UserComment where schema    = gSchema
instance FromJSON   UserComment where parseJSON = gparseJson
instance ToJSON     UserComment where toJSON    = gtoJson
