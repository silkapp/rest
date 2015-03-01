{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , EmptyDataDecls
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}
module Rest.Types.Error
  ( DataError (..)
  , DomainReason (..)
  , Status (..)
  , fromEither
  , toEither
  , Reason_
  , Reason (..)
  , SomeReason (..)
  , ToResponseCode (..)
  ) where

import Control.Applicative (Applicative (..))
import Control.Monad (ap)
import Data.Aeson hiding (Success)
import Data.Foldable (Foldable)
import Data.JSON.Schema (JSONSchema (..), gSchema)
import Data.Traversable (Traversable)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml
import qualified Data.JSON.Schema as JSONSchema

import Rest.Types.Void

-- Error utilities.

data DataError
  = ParseError        String
  | PrintError        String
  | MissingField      String
  | UnsupportedFormat String
  deriving (Eq, Generic, Show)

newtype DomainReason a = DomainReason { reason :: a }
  deriving (Eq, Generic, Functor, Foldable, Show, Traversable)

instance XmlPickler a => XmlPickler (DomainReason a) where
  xpickle = xpWrap (DomainReason, reason) xpickle

instance ToJSON a => ToJSON (DomainReason a) where
  toJSON (DomainReason e) = toJSON e
instance FromJSON a => FromJSON (DomainReason a) where
  parseJSON = fmap DomainReason . parseJSON

instance JSONSchema a => JSONSchema (DomainReason a) where
  schema = schema . fmap reason

data Status a b = Failure a | Success b
  deriving (Eq, Show, Generic, Typeable, Functor, Foldable, Traversable)

instance (XmlPickler a, XmlPickler b) => XmlPickler (Status a b) where
  xpickle = gxpickle

instance (ToJSON   a, ToJSON   b) => ToJSON   (Status a b) where toJSON    = gtoJson
instance (FromJSON a, FromJSON b) => FromJSON (Status a b) where parseJSON = gparseJson

instance (JSONSchema a, JSONSchema b) => JSONSchema (Status a b) where
  schema = gSchema

fromEither :: Either a b -> Status a b
fromEither = either Failure Success

toEither :: Status a b -> Either a b
toEither (Success x) = Right x
toEither (Failure y) = Left  y

type Reason_ = Reason Void

data Reason a
  -- Thrown in the router.
  = UnsupportedRoute
  | UnsupportedMethod
  | UnsupportedVersion

  -- Thrown during generic IO.
  | IdentError   DataError
  | HeaderError  DataError
  | ParamError   DataError
  | InputError   DataError
  | OutputError  DataError

  -- Generic errors thrown in specific handlers.
  | NotFound
  | NotAllowed
  | AuthenticationFailed
  | Busy
  | Gone

  -- Custom domain reasons.
  | CustomReason (DomainReason a)
  deriving (Eq, Generic, Show, Typeable, Functor, Foldable, Traversable)

instance Applicative Reason where
  pure = return
  (<*>) = ap

instance Monad Reason where
  return a = CustomReason (DomainReason a)
  r >>= f = case r of
    CustomReason (DomainReason a) -> f a
    UnsupportedRoute              -> UnsupportedRoute
    UnsupportedMethod             -> UnsupportedMethod
    UnsupportedVersion            -> UnsupportedVersion
    IdentError   e                -> IdentError   e
    HeaderError  e                -> HeaderError  e
    ParamError   e                -> ParamError   e
    InputError   e                -> InputError   e
    OutputError  e                -> OutputError  e
    NotFound                      -> NotFound
    NotAllowed                    -> NotAllowed
    AuthenticationFailed          -> AuthenticationFailed
    Busy                          -> Busy
    Gone                          -> Gone

instance XmlPickler DataError where xpickle = gxpickle
instance XmlPickler e => XmlPickler (Reason e) where xpickle = gxpickle

instance ToJSON DataError where toJSON = gtoJson
instance FromJSON DataError where parseJSON = gparseJson
instance ToJSON e => ToJSON (Reason e) where toJSON = gtoJson
instance FromJSON e => FromJSON (Reason e) where parseJSON = gparseJson

instance JSONSchema DataError where schema = gSchema
instance JSONSchema e => JSONSchema (Reason e) where schema = gSchema

data SomeReason where
  SomeReason :: (XmlPickler e, JSONSchema e, ToJSON e) => Reason e -> SomeReason

deriving instance Typeable SomeReason

instance XmlPickler SomeReason where
  xpickle = PU
    (\(SomeReason e) st -> appPickle xpickle e st)
    (throwMsg "Cannot unpickle SomeReason.")
    Any

instance ToJSON SomeReason where toJSON (SomeReason r) = toJSON r

instance JSONSchema SomeReason where
  schema _ = JSONSchema.Any

-- | The response code that should be given for a type.
-- This is currently only used for errors.
class ToResponseCode a where
  toResponseCode :: a -> Int

instance ToResponseCode Void where
  toResponseCode = magic

instance ToResponseCode a => ToResponseCode (Reason a) where
  toResponseCode e =
    case e of
      NotFound                          -> 404
      UnsupportedRoute                  -> 404
      UnsupportedMethod                 -> 405
      UnsupportedVersion                -> 404
      NotAllowed                        -> 403
      AuthenticationFailed              -> 401
      Busy                              -> 503
      Gone                              -> 410
      OutputError (UnsupportedFormat _) -> 406
      InputError  _                     -> 400
      OutputError _                     -> 500
      IdentError  _                     -> 400
      HeaderError _                     -> 400
      ParamError  _                     -> 400
      CustomReason (DomainReason a)     -> toResponseCode a
