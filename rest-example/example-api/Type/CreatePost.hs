{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.CreatePost where

import Prelude.Compat

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

type Title = Text

data CreatePost = CreatePost
  { title   :: Title
  , content :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler CreatePost where xpickle   = gxpickle
instance JSONSchema CreatePost where schema    = gSchema
instance FromJSON   CreatePost where parseJSON = gparseJson
instance ToJSON     CreatePost where toJSON    = gtoJson
