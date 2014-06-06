{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.UserSignupError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

data UserSignupError = InvalidPassword | InvalidUserName
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''UserSignupError "PFUserSignupError"
type instance PF UserSignupError = PFUserSignupError

instance XmlPickler UserSignupError where xpickle = gxpickle
instance JSONSchema UserSignupError where schema = gSchema
instance FromJSON   UserSignupError
instance ToJSON     UserSignupError
