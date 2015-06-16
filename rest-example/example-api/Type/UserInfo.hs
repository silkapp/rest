{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.UserInfo where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.User as User

data UserInfo = UserInfo
  { name :: User.Name
  } deriving (Generic, Show, Typeable)

instance XmlPickler UserInfo where xpickle   = gxpickle
instance JSONSchema UserInfo where schema    = gSchema
instance FromJSON   UserInfo where parseJSON = gparseJson
instance ToJSON     UserInfo where toJSON    = gtoJson
