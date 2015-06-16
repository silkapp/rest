{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.User where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

type Name = Text
type Password = Text

data User = User
  { name     :: Name
  , password :: Password
  } deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler User where xpickle   = gxpickle
instance JSONSchema User where schema    = gSchema
instance FromJSON   User where parseJSON = gparseJson
instance ToJSON     User where toJSON    = gtoJson
-- We might want to skip the ToJSON instance so we don't accidentally
-- serve passwords, but this type is accepted on signup which means a
-- haskell client needs to be able to serialize it.
