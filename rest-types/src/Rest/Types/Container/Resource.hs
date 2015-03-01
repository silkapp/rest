{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}
module Rest.Types.Container.Resource
  ( Resource (..)
  , Resources (..)

  , KeyValues
  , Value (..)
  ) where

import Data.Aeson hiding (Value)
import Data.CaseInsensitive (CI)
import Data.JSON.Schema (JSONSchema (..), gSchema)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler (gxpickle)
import Rest.StringMap.HashMap.Strict (StringHashMap)
import Rest.Types.Method
import Text.XML.HXT.Arrow.Pickle
import qualified Data.JSON.Schema.Combinators as Json

type KeyValues = StringHashMap String Value
type CaseInsensitiveKeyValues = StringHashMap (CI String) Value

newtype Value = Value { unValue :: String }
  deriving (Show, Typeable)

deriving instance ToJSON Value
deriving instance FromJSON Value
instance JSONSchema Value where
  schema _ = Json.value

instance XmlPickler Value where
  xpickle = xpElem "value" $ xpWrap (Value, unValue) xpText0

data Resource = Resource
  { uri        :: String
  , method     :: Method
  , headers    :: CaseInsensitiveKeyValues
  , parameters :: KeyValues
  , input      :: String
  } deriving (Generic, Show, Typeable)

instance XmlPickler Resource where
  xpickle = gxpickle

instance ToJSON     Resource where toJSON    = gtoJson
instance FromJSON   Resource where parseJSON = gparseJson
instance JSONSchema Resource where schema    = gSchema

-------------------------------------------------------------------------------

newtype Resources = Resources [Resource] deriving (Generic, Typeable)

instance XmlPickler Resources where
  xpickle = gxpickle

instance ToJSON     Resources where toJSON    = gtoJson
instance FromJSON   Resources where parseJSON = gparseJson
instance JSONSchema Resources where schema    = gSchema
