{-# LANGUAGE
    GADTs
  , StandaloneDeriving
  , TemplateHaskell
  , TypeOperators
  , FlexibleContexts
  #-}
module Rest.Dictionary.Types
  (
  -- * Possible I/O formats.

    Format (..)

  -- * The dictionary type.

  , Dict
  , headers
  , params
  , inputs
  , outputs
  , errors

  , empty
  , Modifier

  -- * Dictionary aspects.

  , Ident (..)
  , Header (..)
  , Param (..)
  , Input (..)
  , Output (..)
  , Error (..)

  -- * Plural dictionaries.

  , Dicts (..)
  , dicts
  , Inputs
  , Outputs
  , Errors
  , SomeError (..)

  )

where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.JSON.Schema
import Data.Label ((:->), lens)
import Data.Label.Derive
import Data.Text.Lazy (Text)
import Data.Typeable
import Network.Multipart (BodyPart)
import Text.XML.HXT.Arrow.Pickle

import Rest.Error
import Rest.Info

-- | The `Format` datatype enumerates all input and output formats we might recognize.

data Format
  = XmlFormat
  | JsonFormat
  | StringFormat
  | FileFormat
  | MultipartFormat
  | NoFormat
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The explicit dictionary `Ident` describes how to translate a resource
-- identifier (originating from a request URI) to a Haskell value. We allow
-- plain `String` identifiers or all Haskell types that have a `Read` instance.

data Ident id where
  ReadId   :: (Info id, Read id, Show id) => Ident id
  StringId ::                                Ident String

deriving instance Show (Ident id)

-- | The explicit dictionary `Header` describes how to translate HTTP request
-- headers to some Haskell value. The first field in the `Header` constructor
-- is a white list of headers we can recognize, used in generic validation and
-- for generating documentation. The second field is a custom parser that can
-- fail with a `DataError` or can produce a some value. When explicitly not
-- interested in the headers we can use `NoHeader`.
--
-- Todo: allow multiple parsers for different headers instead of combining them
-- into one parser + use `Info` class to enfore some documention about the
-- parsed type.

data Header h where
  NoHeader ::                                                       Header ()
  Header   :: [String] -> ([Maybe String] -> Either DataError h) -> Header h

-- | The explicit dictionary `Parameter` describes how to translate the request
-- parameters to some Haskell value. The first field in the `Header`
-- constructor is a white list of paramters we can recognize, used in generic
-- validation and for generating documentation. The second field is a custom
-- parser that can fail with a `DataError` or can produce a some value. When
-- explicitly not interested in the parameters we can use `NoParam`.
--
-- Todo: allow multiple parsers for different parameters instead of combining
-- them into one parser + use `Info` class to enfore some documention about the
-- parsed type.

data Param p where
  NoParam   ::                                                       Param ()
  Param     :: [String] -> ([Maybe String] -> Either DataError p) -> Param p
  TwoParams :: Param p -> Param q                                 -> Param (p, q)

-- | The explicitly dictionary `Input` describes how to translate the request
-- body into some Haskell value. We currently use a constructor for every
-- combination of input type to output type. For example, we can use XML input
-- in multiple ways, parsed, as plain/text or as raw bytes, depending on the
-- needs of the backend resource.

data Input i where
  JsonI    :: (Typeable i, FromJSON i, JSONSchema i) => Input i
  ReadI    :: (Info i, Read i, Show i)               => Input i
  StringI  ::                                           Input String
  FileI    ::                                           Input ByteString
  XmlI     :: (Typeable i, XmlPickler i)             => Input i
  XmlTextI ::                                           Input Text
  RawXmlI  ::                                           Input ByteString

deriving instance Show (Input i)
deriving instance Eq   (Input i)
deriving instance Ord  (Input i)

-- | The explicitly dictionary `Output` describes how to translate some Haskell
-- value to a response body. We currently use a constructor for every
-- combination of input type to output type.

data Output o where
  FileO      ::                                         Output (ByteString, String)
  RawXmlO    ::                                         Output ByteString
  JsonO      :: (Typeable o, ToJSON o, JSONSchema o) => Output o
  XmlO       :: (Typeable o, XmlPickler o)           => Output o
  StringO    ::                                         Output String
  MultipartO ::                                         Output [BodyPart]

deriving instance Show (Output o)
deriving instance Eq   (Output o)
deriving instance Ord  (Output o)

-- | The explicitly dictionary `Error` describes how to translate some Haskell
-- error value to a response body.

data Error e where
  JsonE   :: (Typeable e, ToJSON e, JSONSchema e) => Error e
  XmlE    :: (Typeable e, XmlPickler e)           => Error e

deriving instance Show (Error e)
deriving instance Eq   (Error e)
deriving instance Ord  (Error e)

type Inputs  i = Dicts Input  i
type Outputs o = Dicts Output o
type Errors  e = Dicts Error  e

data Dicts f a where
  None  :: Dicts f ()
  Dicts :: [f a] -> Dicts f a

deriving instance Show (f a) => Show (Dicts f a)

dicts :: Dicts f a :-> [f a]
dicts = lens getDicts modDicts
  where
    getDicts None       = []
    getDicts (Dicts ds) = ds
    modDicts :: ([f a] -> [f a]) -> Dicts f a -> Dicts f a
    modDicts _ None       = None
    modDicts f (Dicts ds) = Dicts (f ds)

-- | The `Dict` datatype containing sub-dictionaries for translation of
-- identifiers (i), headers (h), parameters (p), inputs (i), outputs (o), and
-- errors (e). Inputs, outputs and errors can have multiple associated
-- dictionaries.

fclabels [d|
  data Dict h p i o e = Dict
    { headers :: Header  h
    , params  :: Param   p
    , inputs  :: Inputs  i
    , outputs :: Outputs o
    , errors  :: Errors  e
    }
  |]

-- | The empty dictionary, recognizing no types.

empty :: Dict () () () () ()
empty = Dict NoHeader NoParam None None None

-- | Custom existential packing an error together with a Reason.

data SomeError where
  SomeError :: Errors e -> Reason e -> SomeError

-- | Type synonym for dictionary modification.

type Modifier h p i o e = Dict () () () () () -> Dict h p i o e
