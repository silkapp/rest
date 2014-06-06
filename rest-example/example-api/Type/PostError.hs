{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.PostError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

data PostError = InvalidTitle | InvalidContent
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''PostError "PFPostError"
type instance PF PostError = PFPostError

instance XmlPickler PostError where xpickle = gxpickle
instance JSONSchema PostError where schema = gSchema
instance FromJSON   PostError
instance ToJSON     PostError
