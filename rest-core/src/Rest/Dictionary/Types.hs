{-# LANGUAGE
    CPP
  , DataKinds
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
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
  , Xml (..)
  , Json (..)

  -- * Plural dictionaries.

  , Dicts (..)
  , dicts
  , getDicts
  , getDicts_
  , modDicts
  , Inputs
  , Outputs
  , Errors
  , SomeError (..)

  , FromMaybe

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
import Rest.Types.Void

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
  ReadId   :: (Info id, Read id) => Ident id
  StringId ::                       Ident String

deriving instance Show (Ident id)

-- | The explicit dictionary `Header` describes how to translate HTTP request
-- headers to some Haskell value. The first field in the `Header` constructor
-- is a white list of headers we can recognize, used in generic validation and
-- for generating documentation. The second field is a custom parser that can
-- fail with a `DataError` or can produce a some value. When explicitly not
-- interested in the headers we can use `NoHeader`.

data Header h where
  NoHeader    ::                                                       Header ()
  Header      :: [String] -> ([Maybe String] -> Either DataError h) -> Header h
  TwoHeaders  :: Header h -> Header k                               -> Header (h,k)

instance Show (Header h) where
  showsPrec _ NoHeader         = showString "NoHeader"
  showsPrec n (Header hs _)    = showParen (n > 9) (showString "Header " . showsPrec 10 hs)
  showsPrec n (TwoHeaders h k) = showParen (n > 9) ( showString "TwoHeaders "
                                                   . showsPrec 10 h
                                                   . showString " "
                                                   . showsPrec 10 k
                                                   )

-- | The explicit dictionary `Param` describes how to translate the request
-- parameters to some Haskell value. The first field in the `Param`
-- constructor is a white list of paramters we can recognize, used in generic
-- validation and for generating documentation. The second field is a custom
-- parser that can fail with a `DataError` or can produce a some value. When
-- explicitly not interested in the parameters we can use `NoParam`.

data Param p where
  NoParam   ::                                                       Param ()
  Param     :: [String] -> ([Maybe String] -> Either DataError p) -> Param p
  TwoParams :: Param p -> Param q                                 -> Param (p, q)

instance Show (Param p) where
  showsPrec _ NoParam         = showString "NoParam"
  showsPrec n (Param ns _)    = showParen (n > 9) (showString "Param " . showsPrec 10 ns)
  showsPrec n (TwoParams p q) = showParen (n > 9) ( showString "TwoParams "
                                                  . showsPrec 10 p
                                                  . showString " "
                                                  . showsPrec 10 q
                                                  )

-- | The explicit dictionary `Input` describes how to translate the request
-- body into some Haskell value. We currently use a constructor for every
-- combination of input type to output type. For example, we can use XML input
-- in multiple ways, parsed, as plain/text or as raw bytes, depending on the
-- needs of the backend resource.

data Input i where
  JsonI          :: (Typeable i, FromJSON i, JSONSchema i) => Input i
  ReadI          :: (Info i, Read i, Show i)               => Input i
  StringI        ::                                           Input String
  FileI          ::                                           Input ByteString
  XmlI           :: (Typeable i, XmlPickler i)             => Input i
  XmlTextI       ::                                           Input Text
  RawJsonI       ::                                           Input ByteString
  RawXmlI        ::                                           Input ByteString
  RawJsonAndXmlI ::                                           Input (Either Json Xml)

deriving instance Show (Input i)
deriving instance Eq   (Input i)
deriving instance Ord  (Input i)

-- | The explicit dictionary `Output` describes how to translate some Haskell
-- value to a response body. We currently use a constructor for every
-- combination of input type to output type.

data Output o where
  FileO          ::                                         Output (ByteString, String, Bool)
  RawJsonO       ::                                         Output ByteString
  RawXmlO        ::                                         Output ByteString
  JsonO          :: (Typeable o, ToJSON o, JSONSchema o) => Output o
  XmlO           :: (Typeable o, XmlPickler o)           => Output o
  StringO        ::                                         Output String
  RawJsonAndXmlO ::                                         Output ByteString
  MultipartO     ::                                         Output [BodyPart]

deriving instance Show (Output o)
deriving instance Eq   (Output o)
deriving instance Ord  (Output o)

-- | Newtype around ByteStrings used in `RawJsonAndXmlI` to add some
-- protection from parsing the input incorrectly.
newtype Xml = Xml { unXml :: ByteString }
  deriving (Eq, Show)

-- | Newtype around ByteStrings used in `RawJsonAndXmlI` to add some
-- protection from parsing the input incorrectly.
newtype Json = Json { unJson :: ByteString }
  deriving (Eq, Show)

-- | The explicit dictionary `Error` describes how to translate some Haskell
-- error value to a response body.

data Error e where
  JsonE   :: (ToResponseCode e, Typeable e, ToJSON e, JSONSchema e) => Error e
  XmlE    :: (ToResponseCode e, Typeable e, XmlPickler e)           => Error e

deriving instance Show (Error e)
deriving instance Eq   (Error e)
deriving instance Ord  (Error e)

type Inputs  i = Dicts Input  i
type Outputs o = Dicts Output o
type Errors  e = Dicts Error  e

data Dicts f a where
  None  :: Dicts f 'Nothing
  Dicts :: [f a] -> Dicts f ('Just a)

-- Needs UndecidableInstances
deriving instance Show (f (FromMaybe Void a)) => Show (Dicts f a)

#if GLASGOW_HASKELL < 708
type family FromMaybe d (m :: Maybe *) :: *
type instance FromMaybe b 'Nothing  = b
type instance FromMaybe b ('Just a) = a
#else
type family FromMaybe d (m :: Maybe *) :: * where
  FromMaybe b Nothing  = b
  FromMaybe b (Just a) = a
#endif

{-# DEPRECATED dicts "The modifier for this lens doesn't do anything when Dicts is None. Use getDicts and modDicts instead." #-}
dicts :: forall a o f. o ~ FromMaybe o a => Dicts f a :-> [f o]
dicts = lens get modify
  where
    get :: Dicts f a -> [f o]
    get None       = []
    get (Dicts ds) = ds
    modify :: ([f o] -> [f o]) -> Dicts f a -> Dicts f a
    modify _ None       = None
    modify f (Dicts ds) = Dicts (f ds)

-- | Get the list of dictionaries. If there are none, you get a [o].
-- If this is too polymorphic, try `getDicts_`.

getDicts :: o ~ FromMaybe o a => Dicts f a -> [f o]
getDicts None       = []
getDicts (Dicts ds) = ds

-- | Get the list of dictionaries. If there are none, you get a [()].
-- Sometimes useful to constraint the types if the element type of the
-- list isn't clear from the context.

getDicts_ :: o ~ FromMaybe () a => Dicts f a -> [f o]
getDicts_ None = []
getDicts_ (Dicts ds) = ds

modDicts :: (FromMaybe o i ~ o) => ([f o] -> [f o]) -> Dicts f i -> Dicts f ('Just o)
modDicts f None       = Dicts (f [])
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
    } deriving Show
  |]

-- | The empty dictionary, recognizing no types.

empty :: Dict () () 'Nothing 'Nothing 'Nothing
empty = Dict NoHeader NoParam None None None

-- | Custom existential packing an error together with a Reason.

data SomeError where
  SomeError :: Errors e -> Reason (FromMaybe Void e) -> SomeError

-- | Type synonym for dictionary modification.

type Modifier h p i o e = Dict () () 'Nothing 'Nothing 'Nothing -> Dict h p i o e
