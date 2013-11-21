{-# LANGUAGE
    DeriveDataTypeable
  , TemplateHaskell
  , TypeFamilies
  , EmptyDataDecls
  #-}
module Rest.Types.Container.Resource
  ( Resource (..)
  , Resources (..)

  , KeyValues
  , Value (..)
  ) where

import Data.Typeable
import Data.JSON.Schema (JSONSchema (..), Json, gSchema)
import Generics.Regular (deriveAll, PF)
import Generics.Regular.JSON
import Generics.Regular.XmlPickler (gxpickle)
import Text.JSON
import Text.XML.HXT.Arrow.Pickle
import qualified Data.JSON.Schema     as Json

import Rest.Types.Container

type KeyValues = StringMap String Value

newtype Value = Value { unValue :: String } deriving (Show, Typeable)

instance XmlPickler Value where
  xpickle = xpWrap (Value, unValue) xpText0

instance JSON Value where
  showJSON = showJSON . unValue
  readJSON = fmap Value . readJSON

instance JSONSchema Value where
  schema _ = Json.Value 0 (-1)

data Resource = Resource
  { uri        :: String
  , headers    :: KeyValues
  , parameters :: KeyValues
  , input      :: String
  } deriving (Show, Typeable)

deriveAll ''Resource "PFResource"
type instance PF Resource = PFResource

instance XmlPickler Resource where
  xpickle = gxpickle

instance JSON Resource where
 showJSON = gshowJSON
 readJSON = greadJSON

instance JSONSchema Resource  where
  schema = gSchema

instance Json Resource

-------------------------------------------------------------------------------

newtype Resources = Resources [Resource] deriving Typeable

deriveAll ''Resources "PFResources"
type instance PF Resources = PFResources

instance XmlPickler Resources where
  xpickle = gxpickle

instance JSON Resources where
 showJSON = gshowJSON
 readJSON = greadJSON

instance JSONSchema Resources  where
  schema = gSchema

instance Json Resources
