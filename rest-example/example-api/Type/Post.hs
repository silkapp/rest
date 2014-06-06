{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.Post where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.User as User

type Title = Text

data Post = Post
  { author      :: User.Name
  , createdTime :: UTCTime
  , title       :: Title
  , content     :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Post "PFPost"
type instance PF Post = PFPost

instance XmlPickler Post where xpickle = gxpickle
instance JSONSchema Post where schema = gSchema
instance ToJSON     Post
instance FromJSON   Post

instance XmlPickler UTCTime where xpickle = xpPrim
instance JSONSchema UTCTime where schema _ = Value 0 (-1)
