{-# LANGUAGE
    DeriveDataTypeable
  , RankNTypes
  #-}
module Rest.Types.Void (Void (..)) where

import Data.Aeson (ToJSON (..))
import Data.JSON.Schema (JSONSchema (..), Schema(Choice))
import Data.Typeable (Typeable)
import Text.XML.HXT.Arrow.Pickle (XmlPickler (..), PU (..))
import Text.XML.HXT.Arrow.Pickle.Schema (Schema(Alt))
import Text.XML.HXT.Arrow.Pickle.Xml (Unpickler (UP))

-- * The @Void@ type.

-- | The 'Void' type is used as the identifier for resources that
-- can't be routed to. It contains no values apart from bottom.

newtype Void = Void { magic :: forall a. a } deriving (Typeable)

instance ToJSON Void where
  toJSON = magic

instance JSONSchema Void where
  schema _ = Choice []

-- | We'd rather not have the unpickler (like we don't have
-- `FromJSON`) but they are packed together. Unpickling gives an
-- error.

instance XmlPickler Void where
  xpickle = PU magic (UP (\st -> (Left ("Cannot unpickle Void.", st), st))) (Alt [])
