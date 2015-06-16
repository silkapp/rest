{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.Comment (Comment (..)) where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Time
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.Post ()
import qualified Type.User as User

data Comment = Comment
  { author      :: User.Name
  , createdTime :: UTCTime
  , content     :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler Comment where xpickle   = gxpickle
instance JSONSchema Comment where schema    = gSchema
instance FromJSON   Comment where parseJSON = gparseJson
instance ToJSON     Comment where toJSON    = gtoJson
