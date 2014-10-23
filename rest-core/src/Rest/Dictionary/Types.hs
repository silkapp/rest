{-# LANGUAGE
    CPP
  , DataKinds
  , DeriveFunctor
  , FlexibleContexts
  , GADTs
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
  -- , contramap
  , Error (..)

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
  , Accept
  , acceptToContentType

  , FromMaybe

  )

where

import Data.ByteString.Lazy (ByteString)
import Data.Label ((:->), lens)
import Data.Label.Derive
import Data.Typeable
import Network.Multipart (ContentType)

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
  ReadId   :: (Info id, Read id, Show id) => Ident id
  StringId ::                                Ident String

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

-- | The explicit dictionary `Parameter` describes how to translate the request
-- parameters to some Haskell value. The first field in the `Header`
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

-- | The explicitly dictionary `Input` describes how to translate the request
-- body into some Haskell value.

type ParseError = String

data Input i = Input
  { parser  :: ByteString -> Either ParseError i
  -- TODO: re-export ContentType for easily defining formatters?
  , accepts :: ContentType -> Bool
  } deriving (Functor)

type Accept = (ContentType, Double)

acceptToContentType :: Accept -> ContentType
acceptToContentType = fst

-- | The explicitly dictionary `Output` describes how to translate some Haskell
-- value to a response body.

data Output o = forall custom. (Typeable custom, Typeable o) => Output
  { printer :: o      -> ByteString
  -- TODO: should this just take a content-type instead? no sense in
  -- giving a single q value.
  , returns :: Accept -> Bool
  , custom :: custom o
  }

-- TODO: Contravariant instance
-- contramap :: (p -> o) -> Output o -> Output p
-- contramap f o = o { printer = printer o . f }

-- | The explicitly dictionary `Error` describes how to translate some Haskell
-- error value to a response body.

data Error e = Error
  { output       :: Output e
  , responseCode :: e -> Int
  }

type Inputs  i = Dicts Input  i
type Outputs o = Dicts Output o
type Errors  e = Dicts Error  e

data Dicts f a where
  None  :: Dicts f Nothing
  Dicts :: [f a] -> Dicts f (Just a)

-- Needs UndecidableInstances
deriving instance Show (f (FromMaybe Void a)) => Show (Dicts f a)

#if GLASGOW_HASKELL < 708
type family FromMaybe d (m :: Maybe *) :: *
type instance FromMaybe b Nothing  = b
type instance FromMaybe b (Just a) = a
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

modDicts :: (FromMaybe o i ~ o) => ([f o] -> [f o]) -> Dicts f i -> Dicts f (Just o)
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
    }
  |]

-- | The empty dictionary, recognizing no types.

empty :: Dict () () Nothing Nothing Nothing
empty = Dict NoHeader NoParam None None None

-- | Custom existential packing an error together with a Reason.

data SomeError where
  SomeError :: Errors e -> Reason (FromMaybe Void e) -> SomeError

-- | Type synonym for dictionary modification.

type Modifier h p i o e = Dict () () Nothing Nothing Nothing -> Dict h p i o e
