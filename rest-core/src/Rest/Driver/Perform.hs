{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Rest.Driver.Perform
  ( Rest (..)
  , failureWriter
  , writeResponse
  , accept
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer
import Data.Aeson.Utils
import Data.Char (isSpace, ord, toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.UUID (UUID)
import Network.Multipart (BodyPart (..), MultiPart (..), showMultipartBody)
import Safe
import System.IO.Unsafe
import System.Random (randomIO)
import Text.Xml.Pickle

import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Label.Total          as L

import Rest.Dictionary (Dict, Dicts (..), Error (..), Errors, Format (..), FromMaybe, Header (..),
                        Input (..), Inputs, Output (..), Outputs, Param (..))
import Rest.Driver.Types
import Rest.Error
import Rest.Handler
import Rest.Types.Void
import qualified Rest.Dictionary   as D
import qualified Rest.Driver.Types as Rest

class (Applicative m, Monad m) => Rest m where
  getHeader       :: String -> m (Maybe String)
  getParameter    :: String -> m (Maybe String)
  getBody         :: m UTF8.ByteString
  getMethod       :: m (Maybe Rest.Method)
  getPaths        :: m [String]
  lookupMimeType  :: String -> m (Maybe String)
  setHeader       :: String -> String -> m ()
  setResponseCode :: Int -> m ()

instance Rest m => Rest (ContT r m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance Rest m => Rest (ExceptT e m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance (Monoid w, Rest m) => Rest (RWST r w s m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance Rest m => Rest (ReaderT r m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance Rest m => Rest (StateT s m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance (Monoid w, Rest m) => Rest (WriterT w m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance Rest m => Rest (IdentityT m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

instance Rest m => Rest (MaybeT m) where
  getHeader       = lift . getHeader
  getParameter    = lift . getParameter
  getBody         = lift getBody
  getMethod       = lift getMethod
  getPaths        = lift getPaths
  lookupMimeType  = lift . lookupMimeType
  setHeader nm    = lift . setHeader nm
  setResponseCode = lift . setResponseCode

writeResponse :: Rest m => RunnableHandler m -> m UTF8.ByteString
writeResponse (RunnableHandler run (GenHandler dict act _)) = do
  res <- runExceptT $ do
    let os = L.get D.outputs dict
    validator os
    inp <- fetchInputs dict
    output <- mapExceptT run (act inp)
    outputWriter os output
  case res of
    Left  er -> failureWriter (L.get D.errors dict) er
    Right r  -> return r

-------------------------------------------------------------------------------
-- Fetching the input resource.

fetchInputs :: Rest m => Dict h p j o e -> ExceptT (Reason (FromMaybe Void e)) m (Env h p (FromMaybe () j))
fetchInputs dict =
  do bs <- getBody
     ct <- parseContentType

     h <- HeaderError `mapE` headers    (L.get D.headers dict)
     p <- ParamError  `mapE` parameters (L.get D.params dict)
     let inputs = L.get D.inputs dict
     j <- InputError  `mapE`
            case inputs of
              None -> return ()
              _    ->
                case ct of
                  Just XmlFormat      -> parser XmlFormat     inputs bs
                  Just JsonFormat     -> parser JsonFormat    inputs bs
                  Just StringFormat   -> parser StringFormat  inputs bs
                  Just FileFormat     -> parser FileFormat    inputs bs
                  Just x              -> throwError (UnsupportedFormat (show x))
                  Nothing | B.null bs -> parser NoFormat inputs bs
                  Nothing             -> throwError (UnsupportedFormat "unknown")
     return (Env h p j)

parseContentType :: Rest m => m (Maybe Format)
parseContentType =
  do ct <- fromMaybe "" <$> getHeader "Content-Type"
     let segs  = concat (take 1 . splitOn ";" <$> splitOn "," ct)
         types = flip concatMap segs $ \ty ->
                   case splitOn "/" ty of
                     ["application", "xml"]          -> [XmlFormat]
                     ["application", "json"]         -> [JsonFormat]
                     ["text",        "xml"]          -> [XmlFormat]
                     ["text",        "json"]         -> [JsonFormat]
                     ["text",        "plain"]        -> [StringFormat]
                     ["application", "octet-stream"] -> [FileFormat]
                     ["application", _             ] -> [FileFormat]
                     ["image",       _             ] -> [FileFormat]
                     _                               -> []
     return (headMay types)

headers :: Rest m => Header h -> ExceptT DataError m h
headers NoHeader      = return ()
headers (Header xs h) = mapM getHeader xs >>= either throwError return . h
headers (TwoHeaders h1 h2) = (,) <$> headers h1 <*> headers h2

parameters :: Rest m => Param p -> ExceptT DataError m p
parameters NoParam      = return ()
parameters (Param xs p) = mapM (lift . getParameter) xs >>= either throwError return . p
parameters (TwoParams p1 p2) = (,) <$> parameters p1 <*> parameters p2

parser :: Monad m => Format -> Inputs j -> B.ByteString -> ExceptT DataError m (FromMaybe () j)
parser NoFormat None       _ = return ()
parser f        None       _ = throwError (UnsupportedFormat (show f))
parser f        (Dicts ds) v = parserD f ds
  where
    parserD :: Monad m => Format -> [D.Input j] -> ExceptT DataError m j
    parserD XmlFormat     (XmlI           : _ ) = case eitherFromXML (UTF8.toString v) of
                                                    Left err -> throwError (ParseError err)
                                                    Right  r -> return r
    parserD XmlFormat     (XmlTextI       : _ ) = return (decodeUtf8 v)
    parserD StringFormat  (ReadI          : _ ) = (throwError (ParseError "Read") `maybe` return) (readMay (UTF8.toString v))
    parserD JsonFormat    (JsonI          : _ ) = case eitherDecodeV v of
                                                    Right a -> return a
                                                    Left  e -> throwError (ParseError e)
    parserD StringFormat  (StringI        : _ ) = return (UTF8.toString v)
    parserD FileFormat    (FileI          : _ ) = return v
    parserD XmlFormat     (RawXmlI        : _ ) = return v
    parserD JsonFormat    (RawJsonI       : _ ) = return v
    parserD JsonFormat    (RawJsonAndXmlI : _ ) = return (Left v)
    parserD XmlFormat     (RawJsonAndXmlI : _ ) = return (Right v)
    parserD t             []                    = throwError (UnsupportedFormat (show t))
    parserD t             (_              : xs) = parserD t xs

-------------------------------------------------------------------------------
-- Failure responses.

failureWriter :: Rest m => Errors e -> Reason (FromMaybe Void e) -> m UTF8.ByteString
failureWriter es err =
  do formats <- accept
     fromMaybeT (printFallback formats) $
       msum (  (tryPrint err                     es   <$> (formats ++ [XmlFormat]))
            ++ (tryPrint (fallbackError formats) None <$> formats                 )
            )
  where
    tryPrint :: forall m e e'. (e ~ FromMaybe Void e', Rest m)
             => Reason e -> Errors e' -> Format -> MaybeT m UTF8.ByteString
    tryPrint e None JsonFormat = printError JsonFormat (toResponseCode e) (encode e)
    tryPrint e None XmlFormat  = printError XmlFormat  (toResponseCode e) (UTF8.fromString (toXML e))
    tryPrint _ None _          = mzero
    tryPrint e (Dicts ds) f = tryPrintD ds f
      where
        tryPrintD :: Rest m => [D.Error e] -> Format -> MaybeT m UTF8.ByteString
        tryPrintD (JsonE   : _ ) JsonFormat = printError JsonFormat (toResponseCode e) (encode e)
        tryPrintD (XmlE    : _ ) XmlFormat  = printError XmlFormat  (toResponseCode e) (UTF8.fromString (toXML e))
        tryPrintD (_       : xs) t          = tryPrintD xs t
        tryPrintD []             _          = mzero

    printError f cd x =
      do contentType f
         setResponseCode cd
         return x

    printFallback fs = printError XmlFormat (toResponseCode (fallbackError fs)) (UTF8.fromString (toXML $ fallbackError fs))

    fallbackError :: [Format] -> Reason_
    fallbackError fs = OutputError (UnsupportedFormat $ intercalate "," $ map formatCT fs)

    formatCT v =
      case v of
        XmlFormat       -> "xml"
        JsonFormat      -> "json"
        StringFormat    -> "text/plain"
        FileFormat      -> "application/octet-stream"
        MultipartFormat -> "multipart/mixed"
        NoFormat        -> "any"

    fromMaybeT def = runMaybeT >=> maybe def return

-------------------------------------------------------------------------------
-- Printing the output resource.

contentType :: Rest m => Format -> m ()
contentType c = setHeader "Content-Type" $
  case c of
    JsonFormat -> "application/json; charset=UTF-8"
    XmlFormat  -> "application/xml; charset=UTF-8"
    _          -> "text/plain; charset=UTF-8"


validator :: forall v m e. Rest m => Outputs v -> ExceptT (Reason e) m ()
validator = tryOutputs try
  where
    try :: Outputs v -> Format -> ExceptT (Last DataError) m ()
    try None NoFormat        = return ()
    try None XmlFormat       = return ()
    try None JsonFormat      = return ()
    try None StringFormat    = return ()
    try None MultipartFormat = return ()
    try None FileFormat      = unsupportedFormat FileFormat
    try (Dicts ds) f = tryD ds f
      where
        tryD :: forall v'. [Output v'] -> Format -> ExceptT (Last DataError) m ()
        tryD (XmlO           : _ ) XmlFormat    = return ()
        tryD (RawXmlO        : _ ) XmlFormat    = return ()
        tryD (JsonO          : _ ) JsonFormat   = return ()
        tryD (RawJsonO       : _ ) JsonFormat   = return ()
        tryD (RawJsonAndXmlO : _ ) JsonFormat   = return ()
        tryD (RawJsonAndXmlO : _ ) XmlFormat    = return ()
        tryD (StringO        : _ ) StringFormat = return ()
        tryD (FileO          : _ ) FileFormat   = return ()
        tryD (MultipartO     : _ ) _            = return () -- Multipart is always ok, subparts can fail.
        tryD []                    t            = unsupportedFormat t
        tryD (_              : xs) t            = tryD xs t

outputWriter :: forall v m e. Rest m => Outputs v -> FromMaybe () v -> ExceptT (Reason e) m UTF8.ByteString
outputWriter outputs v = tryOutputs try outputs
  where
    try :: Outputs v -> Format -> ExceptT (Last DataError) m UTF8.ByteString
    try None NoFormat        = contentType NoFormat >> ok ""
    try None XmlFormat       = contentType NoFormat >> ok "<done/>"
    try None JsonFormat      = contentType NoFormat >> ok "{}"
    try None StringFormat    = contentType NoFormat >> ok "done"
    try None FileFormat      = unsupportedFormat FileFormat
    try None MultipartFormat = contentType NoFormat >> ok ""
    try (Dicts ds) f = tryD ds f
      where
        tryD :: forall v'. FromMaybe () v ~ v' => [Output v'] -> Format -> ExceptT (Last DataError) m UTF8.ByteString
        tryD (XmlO           : _ ) XmlFormat    = contentType XmlFormat    >> ok (UTF8.fromString (toXML v))
        tryD (RawXmlO        : _ ) XmlFormat    = contentType XmlFormat    >> ok v
        tryD (JsonO          : _ ) JsonFormat   = contentType JsonFormat   >> ok (encode v)
        tryD (RawJsonO       : _ ) JsonFormat   = contentType JsonFormat   >> ok v
        tryD (RawJsonAndXmlO : _ ) JsonFormat   = contentType JsonFormat   >> ok (fst v)
        tryD (RawJsonAndXmlO : _ ) XmlFormat    = contentType XmlFormat    >> ok (snd v)
        tryD (StringO        : _ ) StringFormat = contentType StringFormat >> ok (UTF8.fromString v)
        tryD (MultipartO     : _ ) _            = outputMultipart v
        tryD (FileO          : _ ) FileFormat   =
          do let (content, filename, isAttachment) = v
                 ext = (reverse . takeWhile (/='.') . reverse) filename
             mime <- fromMaybe "application/octet-stream" <$> lookupMimeType (map toLower ext)
             setHeader "Content-Type" mime
             setHeader "Cache-Control" "max-age=604800"
             setHeader "Content-Disposition" (  (if isAttachment then "attachment; " else "")
                                             ++ "filename=\"" ++ headerEscape filename ++ "\""
                                             )
             ok content
        tryD []                t            = unsupportedFormat t
        tryD (_          : xs) t            = tryD xs t
    ok r = setResponseCode 200 >> return r
    -- Escape double quotes with a backslash, since we quote the
    -- values with double quotes, and filter out characters below
    -- space. The latter prevents e.g. newlines ending up in the file
    -- name, which can cause 'response splitting' security issues.
    headerEscape :: String -> String
    headerEscape = filter ((>=32) . ord) . intercalate "\\\"" . splitOn "\""

unsupportedFormat :: (Monad m, Show a) => a -> ExceptT (Last DataError) m a1
unsupportedFormat = throwError . Last . Just . UnsupportedFormat . show

tryOutputs :: Rest m => (t -> Format -> ExceptT (Last DataError) m a) -> t -> ExceptT (Reason e) m a
tryOutputs try outputs = do
  formats <- lift accept
  rethrowLast $ (msum $ try outputs <$> formats) <|> unsupportedFormat formats
  where
    rethrowLast :: Monad m => ExceptT (Last DataError) m a -> ExceptT (Reason e) m a
    rethrowLast = either (maybe (error "Rest.Driver.Perform: ExceptT threw Last Nothing, this is a bug") (throwError . OutputError) . getLast) return <=< lift . runExceptT

outputMultipart :: Rest m => [BodyPart] -> m UTF8.ByteString
outputMultipart vs =
  do let boundary = show $ unsafePerformIO (randomIO :: IO UUID)
     setHeader "Content-Type" ("multipart/mixed; boundary=" ++ boundary)
     return $ showMultipartBody boundary (MultiPart vs)

accept :: Rest m => m [Format]
accept =
  do acceptHeader <- getHeader "Accept"
     ct <- parseContentType
     ty <- fromMaybe "" <$> getParameter "type"
     let fromQuery =
           case ty of
             "json" -> [JsonFormat]
             "xml"  -> [XmlFormat]
             _      -> []
         fromAccept = maybe (allFormats ct) (splitter ct) acceptHeader
     return (fromQuery ++ fromAccept)

  where
    allFormats ct = (maybe id (:) ct) [minBound .. maxBound]
    splitter ct hdr = nub (match ct =<< takeWhile (/= ';') . trim <$> splitOn "," hdr)

    match ct ty =
      case map trim <$> (splitOn "+" . trim <$> splitOn "/" ty) of
        [ ["*"]           , ["*"] ] -> allFormats ct
        [ ["*"]                   ] -> allFormats ct
        [ ["text"]        , xs    ] -> xs >>= txt
        [ ["application"] , xs    ] -> xs >>= app
        [ ["image"]       , xs    ] -> xs >>= img
        _                           -> []

    trim = f . f
      where f = reverse . dropWhile isSpace

    txt "*"            = [XmlFormat, JsonFormat, StringFormat]
    txt "json"         = [JsonFormat]
    txt "xml"          = [XmlFormat]
    txt "plain"        = [StringFormat]
    txt _              = []
    app "*"            = [XmlFormat, JsonFormat, FileFormat]
    app "xml"          = [XmlFormat]
    app "json"         = [JsonFormat]
    app "octet-stream" = [FileFormat]
    app _              = []
    img _              = [FileFormat]
