{-# LANGUAGE GADTs, KindSignatures, TupleSections, DeriveDataTypeable #-}
module Rest.Action where

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad.Error
import Data.ByteString.Lazy (ByteString)
import Data.Typeable
import Rest.ReadInfo
import Data.JSON.Schema
import Data.Text.Lazy (Text)
import Safe
import Text.XML.HXT.Arrow.Pickle

import Rest.Error

-- Generic existential types.

data Some  f where Some  :: f (a :: *     ) -> Some  f
data Some1 f where Some1 :: f (a :: * -> *) -> Some1 f

-- Input and output formats.

data Format
  = XmlFormat
  | JsonFormat
  | StringFormat
  | FileFormat
  | NoFormat
  deriving (Eq, Ord, Enum, Bounded, Show)

data Ident i where
  NoId     ::                                                 Ident ()
  ReadId   :: (Typeable i, ReadInfo i, Read i, Show i)     => Ident i
  StringId ::                                                 Ident String

data Input j where
  JsonI    :: (Typeable j, Json j)                     => Input j
  NoI      ::                                             Input ()
  ReadI    :: (Typeable j, ReadInfo j, Read j, Show j) => Input j
  StringI  ::                                             Input String
  FileI    ::                                             Input ByteString
  XmlI     :: (Typeable j, XmlPickler j)               => Input j
  XmlTextI ::                                             Input Text
  RawXmlI  ::                                             Input ByteString

data Header h where
  NoHeader ::                                           Header ()
  Header   :: [String]
           -> ([Maybe String] -> Either DataError h) -> Header h

data Param p where
  NoParam  ::                                           Param ()
  Param    :: [String]
           -> ([Maybe String] -> Either DataError p) -> Param p

data Output o where
  FileO    ::                               Output (ByteString, String)
  RawXmlO  ::                               Output ByteString
  JsonO    :: (Typeable o, Json o)       => Output o
  NoO      ::                               Output ()
  XmlO     :: (Typeable o, XmlPickler o) => Output o
  StringO  ::                               Output String

data DomainError e where
  NoE     ::                                DomainError ()
  JsonE   :: (Typeable e, Json e)        => DomainError e
  XmlE    :: (Typeable e, XmlPickler e)  => DomainError e

type Inputs  j = [Input  j]
type Outputs o = [Output o]
type Errors  e = [DomainError e]

data SomeError where
  SomeError :: Errors e -> Reason e -> SomeError

-- A dictionary describes the input and output formats applicable to specific
-- API handlers. Every dictionary describes what input types to expect for both
-- the resource identifier from the URI and the request body and the type for
-- the output response.

type Dict i h p j o e =
  ( Ident   i
  , Header  h
  , Param   p
  , Inputs  j
  , Outputs o
  , Errors  e
  )

type Modifier i h p j o e = Dict () () () () () () -> Dict i h p j o e

empty :: Dict () () () () () ()
empty = (NoId, NoHeader, NoParam, [NoI], [NoO], [NoE])

-------------------------------------------------------------------------------
-- Some composable dictionaries.

-- Identifier dictionaries.

stringId :: Dict i h p j o e -> Dict String h p j o e
stringId (_, b, c, d, e, f) = (StringId, b, c, d, e, f)

readId :: (Typeable i, ReadInfo i, Read i, Show i) => Dict x h p j o e -> Dict i h p j o e
readId (_, b, c, d, e, f) = (ReadId, b, c, d, e, f)

-- Input dictionaries.

someI :: Dict i h p () o e -> Dict i h p j o e
someI (a, b, c, _, e, f) = (a, b, c, [], e, f)

stringI :: Dict i h p j o e -> Dict i h p String o e
stringI (a, b, c, _, e, f) = (a, b, c, [StringI], e, f)

xmlTextI :: Dict i h p j o e -> Dict i h p Text o e
xmlTextI (a, b, c, _, e, f) = (a, b, c, [XmlTextI], e, f)

fileI :: Dict i h p j o e -> Dict i h p ByteString o e
fileI (a, b, c, _, e, f) = (a, b, c, [FileI], e, f)

readI :: (Typeable j, ReadInfo j, Read j, Show j) => Dict i h p j o e -> Dict i h p j o e
readI (a, b, c, d, e, f) = (a, b, c, ReadI : d, e, f)

xmlI :: (Typeable j, XmlPickler j) => Dict i h p j o e -> Dict i h p j o e
xmlI (a, b, c, d, e, f) = (a, b, c, XmlI : d, e, f)

rawXmlI :: Dict i h p j o e -> Dict i h p ByteString o e
rawXmlI (a, b, c, _, e, f) = (a, b, c, [RawXmlI], e, f)

jsonI :: (Typeable j, Json j) => Dict i h p j o e -> Dict i h p j o e
jsonI (a, b, c, d, e, f) = (a, b, c, JsonI : d, e, f)

-- Header dictionaries.

mkHeader :: Header h -> Dict i x p j o e -> Dict i h p j o e
mkHeader h (a, _, c, d, e, f) = (a, h, c, d, e, f)

-- Parameter dictionaries.

mkPar :: Param p -> Dict i h x j o e -> Dict i h p j o e
mkPar p (a, b, _, d, e, f) = (a, b, p, d, e, f)

-- Output.

someO :: Dict i h p j () e -> Dict i h p j o e
someO (a, b, c, d, _, f) = (a, b, c, d, [], f)

stringO :: Dict i h p j () e -> Dict i h p j String e
stringO (a, b, c, d, _, f) = (a, b, c, d, [StringO], f)

fileO :: Dict i h p j o e -> Dict i h p j (ByteString, String) e
fileO (a, b, c, d, _, f) = (a, b, c, d, [FileO], f)

xmlO :: (Typeable o, XmlPickler o) => Dict i h p j o e -> Dict i h p j o e
xmlO (a, b, c, d, e, f) = (a, b, c, d, XmlO : e, f)

rawXmlO :: Dict i h p j () e -> Dict i h p j ByteString e
rawXmlO (a, b, c, d, _, f) = (a, b, c, d, [RawXmlO], f)

jsonO :: (Typeable o, Json o) => Dict i h p j o e -> Dict i h p j o e
jsonO (a, b, c, d, e, f) = (a, b, c, d, JsonO : e, f)

-- Error

someE :: (Typeable e, Json e) => Dict i h p j o () -> Dict i h p j o e
someE (a, b, c, d, e, _) = (a, b, c, d, e, [])

jsonE :: (Typeable e, Json e) => Dict i h p j o e -> Dict i h p j o e
jsonE (a, b, c, d, e, f) = (a, b, c, d, e, JsonE : f)

xmlE :: (Typeable e, XmlPickler e) => Dict i h p j o e -> Dict i h p j o e
xmlE (a, b, c, d, e, f) = (a, b, c, d, e, XmlE : f)

-- Composed.

xmlJsonI :: (Typeable j, Json j, XmlPickler j) => Dict i h p () o e -> Dict i h p j o e
xmlJsonI = xmlI . jsonI . someI

xmlJsonO :: (Typeable o, Json o, XmlPickler o) => Dict i h p j () e -> Dict i h p j o e
xmlJsonO = xmlO . jsonO . someO

xml :: (Typeable j, Typeable o, XmlPickler j, XmlPickler o) => Dict i h p () () e -> Dict i h p j o e
xml = xmlI . someI . xmlO . someO

xmlJsonE :: (Typeable e, Json e, XmlPickler e) => Dict i h p j o () -> Dict i h p j o e
xmlJsonE = xmlE . jsonE . someE

xmlJson :: (Typeable j, Typeable o, Json j, Json o, XmlPickler j, XmlPickler o) => Dict i h p () () e -> Dict i h p j o e
xmlJson = xmlJsonI . xmlJsonO

-------------------------------------------------------------------------------

-- todo: update doc.
--
-- Actions take an input of some format to an output of some format, possible
-- producing some error. Because we don't really care about the types for the
-- input, output and error types on the outside, we use an existential type to
-- hide them. We use generic input/output dictionaries to allow conversions
-- from/to different IO types. Only the context the actions runs in `m' is
-- visible.

data Env i h p j = Env
  { ident  :: i
  , header :: h
  , param  :: p
  , input  :: j
  }

data Handler m a where
  Handler :: Monad m =>
    { dictionary :: Dict i h p j o e
    , prepare    :: a -> ErrorT (Reason e) m o
    , action     :: Env i h p j -> ErrorT (Reason e) m a
    , secure     :: Bool
    } -> Handler m a

mkHandler :: Monad m => Modifier i h p j o e -> (a -> ErrorT (Reason e) m o) -> (Env i h p j -> ErrorT (Reason e) m a) -> Handler m a
mkHandler d p a = Handler (d empty) p a False

data Action m where Action :: Handler m a -> Action m

secureHandler :: Handler m a -> Handler m a
secureHandler h = h { secure = True }

secureAction :: Action m -> Action m
secureAction (Action h) = Action (secureHandler h)

mkListing
  :: Monad m
  => Modifier () () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> ((Int, Int) -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkListing d i a = mkHandler (mkPar range . d) i (a . param)

mkListingBy
  :: Monad m
  => Modifier i () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> (Env i () (Int, Int) () -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkListingBy d i a = mkHandler (mkPar range . d) i a

range :: Param (Int, Int)
range = Param ["offset", "count"] $ \xs ->
  maybe (Left (ParseError "range"))
        (Right . normalize)
    $ case xs of
        [Just o, Just c] -> (,)    <$> readMay o <*> readMay c
        [_     , Just c] -> (0,)   <$> readMay c
        [Just o, _     ] -> (,100) <$> readMay o
        _                -> Just (0, 100)
  where normalize = (max 0 *** (min 1000 . max 0))

mkOrderedListing
  :: Monad m
  => Modifier () () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> ((Int, Int, Maybe String, Maybe String) -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkOrderedListing d i a = mkHandler (mkPar orderedRange . d) i (a . param)

mkOrderedListingBy
  :: Monad m
  => Modifier i () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> (Env i () (Int, Int, Maybe String, Maybe String) () -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkOrderedListingBy d i a = mkHandler (mkPar orderedRange . d) i a

orderedRange :: Param (Int, Int, Maybe String, Maybe String)
orderedRange = Param ["offset", "count", "order", "direction"] $ \xs ->
  case xs of
    [mo, mc, mor, md] ->
      maybe (Left (ParseError "range"))
            (Right . (\(o, c) -> (o, c, mor, md)) . normalize)
        $ case (mo, mc) of
            (Just o, Just c) -> (,)    <$> readMay o <*> readMay c
            (_     , Just c) -> (0,)   <$> readMay c
            (Just o, _     ) -> (,100) <$> readMay o
            _                -> Just (0, 100)
    _ -> error "Internal error in orderedRange rest parameters"
  where normalize = (max 0 *** (min 1000 . max 0))

mkCreate :: Monad m => Modifier () () () j o e -> (a -> ErrorT (Reason e) m o) -> (j -> ErrorT (Reason e) m a) -> Action m
mkCreate d i a = Action (mkHandler d i (a . input))

mkUpdate :: Monad m => Modifier i h p j o e -> (a -> ErrorT (Reason e) m o) -> (Env i h p j -> ErrorT (Reason e) m a) -> Action m
mkUpdate d i a = Action (mkHandler d i a)

mkGetter :: Monad m => Modifier i () () () o e -> (a -> ErrorT (Reason e) m o) -> (i -> ErrorT (Reason e) m a) -> Handler m a
mkGetter d i a = mkHandler d i (a . ident)

mkGetterEnv :: Monad m => Modifier i h p j o e -> (a -> ErrorT (Reason e) m o) -> (Env i h p j -> ErrorT (Reason e) m a) -> Handler m a
mkGetterEnv d i a = mkHandler d i a

mkAction :: Monad m => Modifier () () () j o e -> (a -> ErrorT (Reason e) m o) -> (j -> ErrorT (Reason e) m a) -> Action m
mkAction d i a = Action (mkHandler d i (a . input))

constHandler :: Monad m => Modifier () () () () o e -> (a -> ErrorT (Reason e) m o) -> ErrorT (Reason e) m a -> Handler m a
constHandler d i a = mkHandler d i (const a)

constAction :: Monad m => Modifier () () () () o e -> (a -> ErrorT (Reason e) m o) -> ErrorT (Reason e) m a -> Action m
constAction d i a = mkAction d i (const a)

