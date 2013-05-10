-- | Module facilitating informative inspection of datatypes.

{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  #-}
module Rest.Info where

import Data.Typeable

-- | Type class representing information about the read/show function on a data
-- type.

class Typeable a => Info a where
  describe :: a -> String
  example  :: a -> String
  example _ = ""

instance Info String where
  describe _ = "string"

instance Info Integer where
  describe _ = "integer"

