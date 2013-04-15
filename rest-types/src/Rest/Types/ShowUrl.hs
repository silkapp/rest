{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           #-}
module Rest.Types.ShowUrl (ShowUrl(..)) where

import Data.UUID

class ShowUrl a where
  showUrl :: a -> String

instance ShowUrl String  where showUrl = id
instance ShowUrl Int     where showUrl = show
instance ShowUrl Integer where showUrl = show
instance ShowUrl UUID    where showUrl = show
