{-# LANGUAGE
    FlexibleInstances
  , TypeSynonymInstances
  #-}
-- | Module facilitating informative inspection of datatypes.
module Rest.Types.Info (Info (..)) where

import Data.Text (Text)
import Data.Typeable

-- | Type class representing information about the read/show function on a data
-- type.
class Typeable a => Info a where
  describe :: proxy a -> String
  example  :: proxy a -> String
  example _ = ""

instance Info String where
  describe _ = "string"

instance Info Text where
  describe _ = "string"

instance Info Int where
  describe _ = "integer"

instance Info Integer where
  describe _ = "integer"
