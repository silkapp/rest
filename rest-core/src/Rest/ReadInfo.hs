{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           #-}
-- | Module facilitating informative inspection of datatypes
module Rest.ReadInfo where

-- | Type class representing information about the read/show function on a data type
class ReadInfo a where
  describeType :: a -> String
  typeExample  :: a -> String

  typeExample _ = ""

instance ReadInfo String where
  describeType _ = "string"

instance ReadInfo Int where
  describeType _ = "number"
