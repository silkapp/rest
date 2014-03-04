{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , EmptyDataDecls
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  #-}
module Rest.Types.Error
  ( DataError(..)
  , DomainReason(..)
  , Status(..)
  , fromEither
  , toEither
  , Reason_
  , Reason(..)
  , SomeReason(..)
  ) where

import Control.Monad.Error
import Data.Aeson hiding (Success)
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular (PF, deriveAll)
import Generics.Regular.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml

-- Error utilities.

data DataError
  = ParseError        String
  | PrintError        String
  | MissingField      String
  | UnsupportedFormat String
  deriving (Eq, Generic, Show)

data DomainReason a = DomainReason { responseCode :: Int, reason :: a } deriving (Eq, Generic)

instance Show a => Show (DomainReason a) where
  showsPrec a (DomainReason _ e) = showParen (a >= 11) (showString "Domain " . showsPrec 11 e)

instance XmlPickler a => XmlPickler (DomainReason a) where
  xpickle = xpWrap (DomainReason (error "No error function defined for DomainReason parsed from JSON"), reason) xpickle

instance ToJSON a => ToJSON (DomainReason a) where
  toJSON (DomainReason _ e) = toJSON e
instance FromJSON a => FromJSON (DomainReason a) where
  parseJSON = fmap (DomainReason (error "No error function defined for DomainReason parsed from JSON")) . parseJSON


instance JSONSchema a => JSONSchema (DomainReason a) where
  schema = schema . fmap reason

data Status a b = Failure a | Success b deriving (Generic, Typeable)

$(deriveAll ''Status "PFStatus")
type instance PF (Status a b) = PFStatus a b

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

type Reason_ = Reason ()

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
  deriving (Eq, Generic, Show, Typeable)

instance Error DataError

instance Error (Reason e)

$(deriveAll ''DataError "PFDataError")
$(deriveAll ''Reason    "PFReason")

type instance PF DataError  = PFDataError
type instance PF (Reason e) = PFReason e

instance XmlPickler DataError  where xpickle = gxpickle
instance XmlPickler e => XmlPickler (Reason e) where xpickle = gxpickle

instance ToJSON DataError where toJSON = gtoJson
instance FromJSON DataError where parseJSON = gparseJson
instance ToJSON e => ToJSON (Reason e) where toJSON = gtoJson
instance FromJSON e => FromJSON (Reason e) where parseJSON = gparseJson

instance JSONSchema DataError where schema = gSchema
instance JSONSchema e => JSONSchema (Reason e) where schema = gSchema

data SomeReason where
  SomeReason :: (XmlPickler e, JSONSchema e, ToJSON e) => Reason e -> SomeReason

instance Error SomeReason

deriving instance Typeable SomeReason

instance XmlPickler SomeReason where
  xpickle = PU
    (\(SomeReason e) st -> appPickle xpickle e st)
    (throwMsg "Cannot unpickle SomeReason.")
    Any

instance ToJSON SomeReason where toJSON (SomeReason r) = toJSON r

instance JSONSchema SomeReason where
  schema _ = Choice [] -- TODO: this should be something like Any
