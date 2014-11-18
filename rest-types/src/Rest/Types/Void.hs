{-# LANGUAGE
    DeriveDataTypeable
  , RankNTypes
  #-}
module Rest.Types.Void (Void (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.JSON.Schema (JSONSchema (..), Schema(Choice))
import Data.Typeable (Typeable)
import Text.XML.HXT.Arrow.Pickle (XmlPickler (..), PU (..))
import Text.XML.HXT.Arrow.Pickle.Schema (Schema(Alt))
import Text.XML.HXT.Arrow.Pickle.Xml (Unpickler (UP))

-- * The @Void@ type.

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a } deriving (Typeable)

-- This instance is needed for generated API clients.

instance FromJSON Void where
  parseJSON = fail "Cannot parse Void in FromJSON."

instance ToJSON Void where
  toJSON = magic

instance JSONSchema Void where
  schema _ = Choice []

instance XmlPickler Void where
  xpickle = PU magic (UP (\st -> (Left ("Cannot unpickle Void.", st), st))) (Alt [])

instance Show Void where
  show = magic
