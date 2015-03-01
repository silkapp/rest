{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.UserPost where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.CreatePost (CreatePost)
import Type.User (User)

data UserPost = UserPost { user :: User, post :: CreatePost }
  deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler UserPost where xpickle = gxpickle
instance JSONSchema UserPost where schema = gSchema
instance FromJSON   UserPost
instance ToJSON     UserPost
