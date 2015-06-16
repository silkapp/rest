{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.Post where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.User as User

type Id = Int
type Title = Text

data Post = Post
  { id          :: Id
  , author      :: User.Name
  , createdTime :: UTCTime
  , title       :: Title
  , content     :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler Post where xpickle   = gxpickle
instance JSONSchema Post where schema    = gSchema
instance FromJSON   Post where parseJSON = gparseJson
instance ToJSON     Post where toJSON    = gtoJson

instance XmlPickler UTCTime where xpickle = xpPrim
