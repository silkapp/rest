{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CreatePost where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

type Title = Text

data CreatePost = CreatePost
  { title   :: Title
  , content :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''CreatePost "PFCreatePost"
type instance PF CreatePost = PFCreatePost

instance XmlPickler CreatePost where xpickle = gxpickle
instance JSONSchema CreatePost where schema = gSchema
instance FromJSON   CreatePost
instance ToJSON     CreatePost
