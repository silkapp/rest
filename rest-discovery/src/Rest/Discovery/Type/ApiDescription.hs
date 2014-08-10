{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  , EmptyDataDecls
  #-}
module Rest.Discovery.Type.ApiDescription where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle
import Rest.Info

type Name = Text

data ApiDescription = ApiDescription
  { name     :: Name
  , link     :: Text
  , subapis  :: [ApiDescription]
  } deriving (Eq, Generic, Ord, Show, Typeable, Read)

deriveAll ''ApiDescription "PFApiDescription"
type instance PF ApiDescription = PFApiDescription

instance XmlPickler ApiDescription where xpickle = gxpickle
instance JSONSchema ApiDescription where schema = gSchema
instance ToJSON     ApiDescription
instance Rest.Info.Info ApiDescription where describe _ = "Api description"
