{-|
A dictionary (`Dict`) describes how to convert Haskell values to and from a web
representation. Rest resources internally use plain Haskell datatypes, while
communication to the outside world mostly happens using XML, JSON, plain text,
query parameters, etc. The `Dict` datatype describes how to convert resource
/indentifiers, input, request parameters, request headers, output, and errors/
to and from a Haskell representation.

The `Dict` datatype and most functions working on it take a type parameters for
every aspect of its communication, which can grow quickly. This module and
most code that depend on it uses the implicit convention of using the type
variable `id` for the resource identifier, the `h` for the request headers, the
`p` for the request parameters, the `i` for the request body, the `o` for the
response body, and the `e` for a possible error.
-}

{-# LANGUAGE
    GADTs
  , KindSignatures
  , TupleSections
  , DeriveDataTypeable
  #-}
module Rest.Dictionary
(

-- * Possible I/O formats.

  Format (..)

-- * The dictionary type.

, Dict
, Modifier

-- * Dictionary aspects.

, Ident (..)
, Header (..)
, Param (..)
, Input (..)
, Output (..)
, Error (..)

-- * Plural dictionaries.

, Inputs
, Outputs
, Errors
, SomeError (..)

-- * Combinators for building dictionaries.

, empty

-- ** Adding identifier types

, stringId
, readId

-- ** Header dictionaries

, mkHeader

-- ** Parameter dictionaries

, mkPar

-- ** Input dictionaries

, someI
, stringI
, xmlTextI
, fileI
, readI
, xmlI
, rawXmlI
, jsonI

-- ** Output dictionaries

, someO
, stringO
, fileO
, xmlO
, rawXmlO
, jsonO

-- ** Error dictionaries

, someE
, jsonE
, xmlE

-- ** Composed dictionaries

, xmlJsonI
, xmlJsonO
, xmlJsonE
, xmlJson
)

where

import Data.ByteString.Lazy (ByteString)
import Data.JSON.Schema
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.XML.HXT.Arrow.Pickle

import Rest.Info
import Rest.Error

-- | The `Format` datatype enumerates all input and output formats we might recognize.

data Format
  = XmlFormat
  | JsonFormat
  | StringFormat
  | FileFormat
  | NoFormat
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The explicit dictionary `Ident` describes how to translate a resource
-- identifier (originating from a request URI) to a Haskell value. We allow
-- plain `String` identifiers or all Haskell types that have a `Read` instance.
-- The absence of an identifier is recognized with `NoId`.

data Ident id where
  NoId     ::                                Ident ()
  ReadId   :: (Info id, Read id, Show id) => Ident id
  StringId ::                                Ident String

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
  NoParam  ::                                                       Param ()
  Param    :: [String] -> ([Maybe String] -> Either DataError p) -> Param p

-- | The explicitly dictionary `Input` describes how to translate the request
-- body into some Haskell value. We currently use a constructor for every
-- combination of input type to output type. For example, we can use XML input
-- in multiple ways, parsed, as plain/text or as raw bytes, depending on the
-- needs of the backend resource.

data Input i where
  JsonI    :: (Typeable i, Json i)       => Input i
  NoI      ::                               Input ()
  ReadI    :: (Info i, Read i, Show i)   => Input i
  StringI  ::                               Input String
  FileI    ::                               Input ByteString
  XmlI     :: (Typeable i, XmlPickler i) => Input i
  XmlTextI ::                               Input Text
  RawXmlI  ::                               Input ByteString

-- | The explicitly dictionary `Output` describes how to translate some Haskell
-- value to a response body. We currently use a constructor for every
-- combination of input type to output type.

data Output o where
  FileO    ::                               Output (ByteString, String)
  RawXmlO  ::                               Output ByteString
  JsonO    :: (Typeable o, Json o)       => Output o
  NoO      ::                               Output ()
  XmlO     :: (Typeable o, XmlPickler o) => Output o
  StringO  ::                               Output String

-- | The explicitly dictionary `Error` describes how to translate some Haskell
-- error value to a response body.

data Error e where
  NoE     ::                                Error ()
  JsonE   :: (Typeable e, Json e)        => Error e
  XmlE    :: (Typeable e, XmlPickler e)  => Error e

-- | The `Dict` datatype containing sub-dictionaries for translation of
-- identifiers (i), headers (h), parameters (p), inputs (i), outputs (o), and
-- errors (e). Inputs, outputs and errors can have multiple associated
-- dictionaries.

type Dict id h p i o e =
  ( Ident   id
  , Header  h
  , Param   p
  , Inputs  i
  , Outputs o
  , Errors  e
  )

-- | Custom existential packing an error together with a Reason.

data SomeError where
  SomeError :: Errors e -> Reason e -> SomeError

type Inputs  i = [Input  i]
type Outputs o = [Output o]
type Errors  e = [Error e]

-- | Type synonym for dictionary modification.

type Modifier id h p i o e = Dict () () () () () () -> Dict id h p i o e

-------------------------------------------------------------------------------

-- | The empty dictionary, recognizing no types.

empty :: Dict () () () () () ()
empty = (NoId, NoHeader, NoParam, [NoI], [NoO], [NoE])

-- | Identifiers are directly usable as `String`.

stringId :: Dict id h p i o e -> Dict String h p i o e
stringId (_, b, c, d, e, f) = (StringId, b, c, d, e, f)

-- | Identifiers are can be read into some instance of `Read`. For inspection
-- reasons the type must also be an instance of both `Info` and `Show`.

readId :: (Info id, Read id, Show id) => Dict x h p i o e -> Dict id h p i o e
readId (_, b, c, d, e, f) = (ReadId, b, c, d, e, f)

-- | Add custom sub-dictionary for recognizing headers.

mkHeader :: Header h -> Dict id x p i o e -> Dict id h p i o e
mkHeader h (a, _, c, d, e, f) = (a, h, c, d, e, f)

-- | Add custom sub-dictionary for recognizing parameters.

mkPar :: Param p -> Dict id h x i o e -> Dict id h p i o e
mkPar p (a, b, _, d, e, f) = (a, b, p, d, e, f)

-- | Open up input type for extension with custom dictionaries.

someI :: Dict id h p () o e -> Dict id h p i o e
someI (a, b, c, _, e, f) = (a, b, c, [], e, f)

-- | Allow direct usage of as input as `String`.

stringI :: Dict id h p i o e -> Dict id h p String o e
stringI (a, b, c, _, e, f) = (a, b, c, [StringI], e, f)

-- | Allow direct usage of as input as raw Xml `Text`.

xmlTextI :: Dict id h p i o e -> Dict id h p Text o e
xmlTextI (a, b, c, _, e, f) = (a, b, c, [XmlTextI], e, f)

-- | Allow usage of input as file contents, represented as a `ByteString`.

fileI :: Dict id h p i o e -> Dict id h p ByteString o e
fileI (a, b, c, _, e, f) = (a, b, c, [FileI], e, f)

-- | The input can be read into some instance of `Read`. For inspection reasons
-- the type must also be an instance of both `Info` and `Show`.

readI :: (Info i, Read i, Show i) => Dict id h p i o e -> Dict id h p i o e
readI (a, b, c, d, e, f) = (a, b, c, ReadI : d, e, f)

-- | The input can be read into some instance of `XmlPickler`.

xmlI :: (Typeable i, XmlPickler i) => Dict id h p i o e -> Dict id h p i o e
xmlI (a, b, c, d, e, f) = (a, b, c, XmlI : d, e, f)

-- | The input can be used as an XML `ByteString`.

rawXmlI :: Dict id h p i o e -> Dict id h p ByteString o e
rawXmlI (a, b, c, _, e, f) = (a, b, c, [RawXmlI], e, f)

-- | The input can be read into some instance of `Json`.

jsonI :: (Typeable i, Json i) => Dict id h p i o e -> Dict id h p i o e
jsonI (a, b, c, d, e, f) = (a, b, c, JsonI : d, e, f)

-- | Open up output type for extension with custom dictionaries.

someO :: Dict id h p i () e -> Dict id h p i o e
someO (a, b, c, d, _, f) = (a, b, c, d, [], f)

-- | Allow output as plain String.

stringO :: Dict id h p i () e -> Dict id h p i String e
stringO (a, b, c, d, _, f) = (a, b, c, d, [StringO], f)

-- | Allow file output using a combination of the raw data and a mime type.

fileO :: Dict id h p i o e -> Dict id h p i (ByteString, String) e
fileO (a, b, c, d, _, f) = (a, b, c, d, [FileO], f)

-- | Allow output as XML using the `XmlPickler` type class.

xmlO :: (Typeable o, XmlPickler o) => Dict id h p i o e -> Dict id h p i o e
xmlO (a, b, c, d, e, f) = (a, b, c, d, XmlO : e, f)

-- | Allow output as raw XML represented as a `ByteString`.

rawXmlO :: Dict id h p i () e -> Dict id h p i ByteString e
rawXmlO (a, b, c, d, _, f) = (a, b, c, d, [RawXmlO], f)

-- | Allow output as JSON using the `Json` type class.

jsonO :: (Typeable o, Json o) => Dict id h p i o e -> Dict id h p i o e
jsonO (a, b, c, d, e, f) = (a, b, c, d, JsonO : e, f)

-- | Open up error type for extension with custom dictionaries.

someE :: (Typeable e, Json e) => Dict id h p i o () -> Dict id h p i o e
someE (a, b, c, d, e, _) = (a, b, c, d, e, [])

-- | Allow error output as JSON using the `Json` type class.

jsonE :: (Typeable e, Json e) => Dict id h p i o e -> Dict id h p i o e
jsonE (a, b, c, d, e, f) = (a, b, c, d, e, JsonE : f)

-- | Allow error output as XML using the `XmlPickler` type class.

xmlE :: (Typeable e, XmlPickler e) => Dict id h p i o e -> Dict id h p i o e
xmlE (a, b, c, d, e, f) = (a, b, c, d, e, XmlE : f)

-- | The input can be read into some instance of both `Json` and `XmlPickler`.

xmlJsonI :: (Typeable i, Json i, XmlPickler i) => Dict id h p () o e -> Dict id h p i o e
xmlJsonI = xmlI . jsonI . someI

-- | Allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJsonO :: (Typeable o, Json o, XmlPickler o) => Dict id h p i () e -> Dict id h p i o e
xmlJsonO = xmlO . jsonO . someO

-- | Allow error output as JSON using the `Json` type class and allow output as
-- XML using the `XmlPickler` type class.

xmlJsonE :: (Typeable e, Json e, XmlPickler e) => Dict id h p i o () -> Dict id h p i o e
xmlJsonE = xmlE . jsonE . someE

-- | The input can be read into some instance of both `Json` and `XmlPickler`
-- and allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJson :: (Typeable i, Typeable o, Json i, Json o, XmlPickler i, XmlPickler o) => Dict id h p () () e -> Dict id h p i o e
xmlJson = xmlJsonI . xmlJsonO

