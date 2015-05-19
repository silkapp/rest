{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
module Type.UserSignupError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data UserSignupError = InvalidPassword | InvalidUserName
  deriving (Eq, Generic, Ord, Show, Typeable)

instance XmlPickler UserSignupError where xpickle = gxpickle
instance JSONSchema UserSignupError where schema = gSchema
instance FromJSON   UserSignupError
instance ToJSON     UserSignupError

instance ToResponseCode UserSignupError where
  toResponseCode _ = 400
