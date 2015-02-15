{-# LANGUAGE
    FlexibleContexts
  , GADTs
  #-}
-- | Error types that can be returned by handlers, as well as some
-- utilities for manipulating these errors.
module Rest.Error
  ( module Rest.Types.Error
  , mapE
  , orThrow
  , orThrowWith
  , eitherToStatus
  , domainReason
  , (>|<)
  ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Functor.Alt
import Data.Functor.Bind (Bind)
import Data.Semigroup

import Rest.Types.Error

-- Error utilities.

infixl 8 `mapE`

mapE :: (Applicative m, Monad m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapE f = mapExceptT (either (Left . f) Right <$>)

orThrow :: MonadError e m => m (Maybe b) -> e -> m b
orThrow a e = a >>= throwError e `maybe` return

orThrowWith :: MonadError a m => m (Either e b) -> (e -> a) -> m b
orThrowWith a f = a >>= (throwError . f) `either` return

eitherToStatus :: Either a b -> Status a b
eitherToStatus (Left  e) = Failure e
eitherToStatus (Right e) = Success e

-- | Wrap your custom error type in a 'Reason'.
-- This requires the ToResponseCode dictionary to pick a response code when
-- the error is served.
domainReason :: ToResponseCode a => a -> Reason a
domainReason = CustomReason . DomainReason

infixl 3 >|<
-- | Combine two ExceptT computations yielding the last error if both fail.
-- This prevents the need for a Semigroup or Monoid instance for the error type, which is necessary if using (<!>) or (<|>) respectively.
(>|<) :: (Bind m, Applicative m, Monad m) => ExceptT e m a -> ExceptT e m a -> ExceptT e m a
a >|< b = mapE getLast (mapE Last a <!> mapE Last b)
