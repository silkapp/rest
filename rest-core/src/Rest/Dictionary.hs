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
  , StandaloneDeriving
  #-}
module Rest.Dictionary
(

-- * Possible I/O formats.

  Format (..)

-- * The dictionary type.

, Dict
, Modifier

-- * Dictionary aspects.

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

-- ** Header dictionaries

, mkHeader

-- ** Parameter dictionaries

, mkPar
, addPar

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
  JsonI    :: (Typeable i, Json i)       => Input i
  NoI      ::                               Input ()
  ReadI    :: (Info i, Read i, Show i)   => Input i
  StringI  ::                               Input String
  FileI    ::                               Input ByteString
  XmlI     :: (Typeable i, XmlPickler i) => Input i
  XmlTextI ::                               Input Text
  RawXmlI  ::                               Input ByteString

deriving instance Show (Input i)

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

deriving instance Show (Output o)

-- | The explicitly dictionary `Error` describes how to translate some Haskell
-- error value to a response body.

data Error e where
  NoE     ::                                Error ()
  JsonE   :: (Typeable e, Json e)        => Error e
  XmlE    :: (Typeable e, XmlPickler e)  => Error e

deriving instance Show (Error e)

-- | The `Dict` datatype containing sub-dictionaries for translation of
-- identifiers (i), headers (h), parameters (p), inputs (i), outputs (o), and
-- errors (e). Inputs, outputs and errors can have multiple associated
-- dictionaries.

type Dict h p i o e =
  ( Header  h
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

type Modifier h p i o e = Dict () () () () () -> Dict h p i o e

-------------------------------------------------------------------------------

-- | The empty dictionary, recognizing no types.

empty :: Dict () () () () ()
empty = (NoHeader, NoParam, [NoI], [NoO], [NoE])

-- | Add custom sub-dictionary for recognizing headers.

mkHeader :: Header h -> Dict x p i o e -> Dict h p i o e
mkHeader h (_, c, d, e, f) = (h, c, d, e, f)

-- | Set custom sub-dictionary for recognizing parameters.

mkPar :: Param p -> Dict h x i o e -> Dict h p i o e
mkPar p (b, _, d, e, f) = (b, p, d, e, f)

-- | Add custom sub-dictionary for recognizing parameters.

addPar :: Param p -> Dict h p' i o e -> Dict h (p, p') i o e
addPar p (b, p', d, e, f) = (b, TwoParams p p', d, e, f)

-- | Open up input type for extension with custom dictionaries.

someI :: Dict h p () o e -> Dict h p i o e
someI (b, c, _, e, f) = (b, c, [], e, f)

-- | Allow direct usage of as input as `String`.

stringI :: Dict h p i o e -> Dict h p String o e
stringI (b, c, _, e, f) = (b, c, [StringI], e, f)

-- | Allow direct usage of as input as raw Xml `Text`.

xmlTextI :: Dict h p i o e -> Dict h p Text o e
xmlTextI (b, c, _, e, f) = (b, c, [XmlTextI], e, f)

-- | Allow usage of input as file contents, represented as a `ByteString`.

fileI :: Dict h p i o e -> Dict h p ByteString o e
fileI (b, c, _, e, f) = (b, c, [FileI], e, f)

-- | The input can be read into some instance of `Read`. For inspection reasons
-- the type must also be an instance of both `Info` and `Show`.

readI :: (Info i, Read i, Show i) => Dict h p i o e -> Dict h p i o e
readI (b, c, d, e, f) = (b, c, ReadI : d, e, f)

-- | The input can be read into some instance of `XmlPickler`.

xmlI :: (Typeable i, XmlPickler i) => Dict h p i o e -> Dict h p i o e
xmlI (b, c, d, e, f) = (b, c, XmlI : d, e, f)

-- | The input can be used as an XML `ByteString`.

rawXmlI :: Dict h p i o e -> Dict h p ByteString o e
rawXmlI (b, c, _, e, f) = (b, c, [RawXmlI], e, f)

-- | The input can be read into some instance of `Json`.

jsonI :: (Typeable i, Json i) => Dict h p i o e -> Dict h p i o e
jsonI (b, c, d, e, f) = (b, c, JsonI : d, e, f)

-- | Open up output type for extension with custom dictionaries.

someO :: Dict h p i () e -> Dict h p i o e
someO (b, c, d, _, f) = (b, c, d, [], f)

-- | Allow output as plain String.

stringO :: Dict h p i () e -> Dict h p i String e
stringO (b, c, d, _, f) = (b, c, d, [StringO], f)

-- | Allow file output using a combination of the raw data and a mime type.

fileO :: Dict h p i o e -> Dict h p i (ByteString, String) e
fileO (b, c, d, _, f) = (b, c, d, [FileO], f)

-- | Allow output as XML using the `XmlPickler` type class.

xmlO :: (Typeable o, XmlPickler o) => Dict h p i o e -> Dict h p i o e
xmlO (b, c, d, e, f) = (b, c, d, XmlO : e, f)

-- | Allow output as raw XML represented as a `ByteString`.

rawXmlO :: Dict h p i () e -> Dict h p i ByteString e
rawXmlO (b, c, d, _, f) = (b, c, d, [RawXmlO], f)

-- | Allow output as JSON using the `Json` type class.

jsonO :: (Typeable o, Json o) => Dict h p i o e -> Dict h p i o e
jsonO (b, c, d, e, f) = (b, c, d, JsonO : e, f)

-- | Open up error type for extension with custom dictionaries.

someE :: (Typeable e, Json e) => Dict h p i o () -> Dict h p i o e
someE (b, c, d, e, _) = (b, c, d, e, [])

-- | Allow error output as JSON using the `Json` type class.

jsonE :: (Typeable e, Json e) => Dict h p i o e -> Dict h p i o e
jsonE (b, c, d, e, f) = (b, c, d, e, JsonE : f)

-- | Allow error output as XML using the `XmlPickler` type class.

xmlE :: (Typeable e, XmlPickler e) => Dict h p i o e -> Dict h p i o e
xmlE (b, c, d, e, f) = (b, c, d, e, XmlE : f)

-- | The input can be read into some instance of both `Json` and `XmlPickler`.

xmlJsonI :: (Typeable i, Json i, XmlPickler i) => Dict h p () o e -> Dict h p i o e
xmlJsonI = xmlI . jsonI . someI

-- | Allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJsonO :: (Typeable o, Json o, XmlPickler o) => Dict h p i () e -> Dict h p i o e
xmlJsonO = xmlO . jsonO . someO

-- | Allow error output as JSON using the `Json` type class and allow output as
-- XML using the `XmlPickler` type class.

xmlJsonE :: (Typeable e, Json e, XmlPickler e) => Dict h p i o () -> Dict h p i o e
xmlJsonE = xmlE . jsonE . someE

-- | The input can be read into some instance of both `Json` and `XmlPickler`
-- and allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJson :: (Typeable i, Typeable o, Json i, Json o, XmlPickler i, XmlPickler o) => Dict h p () () e -> Dict h p i o e
xmlJson = xmlJsonI . xmlJsonO

