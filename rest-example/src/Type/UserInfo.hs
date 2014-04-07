{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.UserInfo where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.User as User

data UserInfo = UserInfo
  { name :: User.Name
  } deriving (Generic, Show, Typeable)

deriveAll ''UserInfo "PFUserInfo"
type instance PF UserInfo = PFUserInfo

instance XmlPickler UserInfo where xpickle = gxpickle
instance JSONSchema UserInfo where schema = gSchema
instance ToJSON     UserInfo
instance FromJSON   UserInfo
