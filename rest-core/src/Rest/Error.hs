{-# LANGUAGE
    TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , DeriveDataTypeable
  #-}
module Rest.Error
  ( module Rest.Types.Error
  , mapE
  , orThrow
  , orThrowWith
  , eitherToStatus
  , domainReason
  ) where

import Control.Applicative
import Control.Monad.Error

import Rest.Types.Error

-- Error utilities.

infixl 8 `mapE`

mapE :: (Applicative m, Monad m) => (e -> e') -> ErrorT e m a -> ErrorT e' m a
mapE f = mapErrorT (either (Left . f) Right <$>)

orThrow :: MonadError e m => m (Maybe b) -> e -> m b
orThrow a e = a >>= throwError e `maybe` return

orThrowWith :: MonadError a m => m (Either e b) -> (e -> a) -> m b
orThrowWith a f = a >>= (throwError . f) `either` return

eitherToStatus :: Either a b -> Status a b
eitherToStatus (Left  e) = Failure e
eitherToStatus (Right e) = Success e

domainReason :: (a -> Int) -> a -> Reason a
domainReason f x = CustomReason (DomainReason (f x) x)
