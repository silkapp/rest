{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.Comment (Comment (..)) where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Time
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.Post ()
import qualified Type.User as User

data Comment = Comment
  { author      :: User.Name
  , createdTime :: UTCTime
  , content     :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Comment "PFComment"
type instance PF Comment = PFComment

instance XmlPickler Comment where xpickle = gxpickle
instance JSONSchema Comment where schema = gSchema
instance FromJSON   Comment
instance ToJSON     Comment
