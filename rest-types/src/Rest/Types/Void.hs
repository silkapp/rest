{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , EmptyDataDecls
  , RankNTypes
  , TypeFamilies
  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DataKinds #-}
#endif
module Rest.Types.Void (Void (..)) where

import Prelude.Compat

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.JSON.Schema (JSONSchema (..), Schema(Choice))
import Data.Typeable (Typeable)
import GHC.Generics
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

instance Eq Void where
  x == _ = magic x

instance Ord Void where
  compare x _ = magic x

instance Read Void where
  readsPrec _ _ = []

-- | Generic. Can't derive it, sadly.

instance Generic Void where
#if __GLASGOW_HASKELL__ >= 800
  type Rep Void = D1 ('MetaData "Void" "Rest.Types.Void" "rest-types" 'False) V1
#else
  type Rep Void = D1 D1Void V1
#endif
  from = magic
  to (M1 x) = x `seq` Void (error "Impossible: constructing a Void in Generic instance.")

#if __GLASGOW_HASKELL__ < 800
data D1Void

instance Datatype D1Void where
  datatypeName _ = "Void"
  moduleName _ = "Rest.Types.Void"
#endif
