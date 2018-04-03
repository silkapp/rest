{-# LANGUAGE
    FlexibleInstances
  , TypeSynonymInstances
  #-}
module Rest.Types.ShowUrl (ShowUrl(..)) where

import Data.UUID.Types
import qualified Data.Text      as ST
import qualified Data.Text.Lazy as LT

class ShowUrl a where
  showUrl :: a -> String

instance ShowUrl String  where showUrl = id
instance ShowUrl Int     where showUrl = show
instance ShowUrl Integer where showUrl = show
instance ShowUrl UUID    where showUrl = show
instance ShowUrl ST.Text where showUrl = ST.unpack
instance ShowUrl LT.Text where showUrl = LT.unpack
