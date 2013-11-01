{-# LANGUAGE RankNTypes
           , GADTs
           , ScopedTypeVariables
           , OverloadedStrings
           #-}
module Rest.Driver.Perform where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer
import Data.Char (isSpace, toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text.Lazy.Encoding (decodeUtf8)
import Safe
import Text.JSON
import Text.Xml.Pickle

import qualified Control.Monad.Error       as E
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Label.Total          as L

import Rest.Dictionary ( Dict, Format (..)
                       , Param (..), Header (..), Input (..), Output (..), Error (..)
                       , Inputs, Outputs, Errors
                       )
import Rest.Driver.Routing (route)
import Rest.Driver.Types
import Rest.Error
import Rest.Handler
import Rest.Resource (Api)
import qualified Rest.Dictionary     as D
import qualified Rest.Driver.Routing as Rest

class (Applicative m, Monad m) => Rest m where
  getHeader       :: String -> m (Maybe String)
  getParameter    :: String -> m (Maybe String)
  getBody         :: m (UTF8.ByteString)
  getMethod       :: m Rest.Method
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

instance (E.Error e, Rest m) => Rest (ErrorT e m) where
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


apiToHandler :: Rest m => Api m -> m UTF8.ByteString
apiToHandler = apiToHandler' id

apiToHandler' :: Rest n => Run m n -> Api m -> n UTF8.ByteString
apiToHandler' run api = do
  method <- getMethod
  paths  <- getPaths
  case route method paths api of
    Left  e                        -> failureWriter [NoE] e
    Right (RunnableHandler run' h) -> writeResponse (RunnableHandler (run . run') h)

writeResponse :: Rest m => RunnableHandler m -> m UTF8.ByteString
writeResponse (RunnableHandler run (GenHandler dict act _)) = do
  res <- runErrorT $ do
    let os = L.get D.outputs dict
    validator os
    inp <- fetchInputs dict
    output <- mapErrorT run (act inp)
    outputWriter os output
  case res of
    Left  er -> failureWriter (L.get D.errors dict) er
    Right r  -> return r

-------------------------------------------------------------------------------
-- Fetching the input resource.

fetchInputs :: Rest m => Dict h p j o e -> ErrorT (Reason e) m (Env h p j)
fetchInputs dict =
  do bs <- getBody
     ct <- parseContentType

     h <- HeaderError `mapE` headers    (L.get D.headers dict)
     p <- ParamError  `mapE` parameters (L.get D.params dict)
     let inputs = L.get D.inputs dict
     j <- InputError  `mapE`
            case inputs of
              [NoI] -> return ()
              _     ->
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

headers :: Rest m => Header h -> ErrorT DataError m h
headers NoHeader      = return ()
headers (Header xs h) = mapM getHeader xs >>= either throwError return . h

parameters :: Rest m => Param p -> ErrorT DataError m p
parameters NoParam      = return ()
parameters (Param xs p) = mapM (lift . getParameter) xs >>= either throwError return . p
parameters (TwoParams p1 p2) = (,) <$> parameters p1 <*> parameters p2

parser :: Monad m => Format -> Inputs j -> B.ByteString -> ErrorT DataError m j
parser XmlFormat     (XmlI     : _ ) v = case eitherFromXML (UTF8.toString v) of
                                           Left err -> throwError (ParseError err)
                                           Right  r -> return r
parser XmlFormat     (XmlTextI : _ ) v = return (decodeUtf8 v)
parser NoFormat      (NoI      : _ ) _ = return ()
parser StringFormat  (ReadI    : _ ) v = (throwError (ParseError "Read") `maybe` return) (readMay (UTF8.toString v))
parser JsonFormat    (JsonI    : _ ) v = case decode (UTF8.toString v) of
                                           Ok a      -> return a
                                           Error msg -> throwError (ParseError msg)
parser StringFormat  (StringI  : _ ) v = return (UTF8.toString v)
parser FileFormat    (FileI    : _ ) v = return v
parser XmlFormat     (RawXmlI  : _ ) v = return v
parser t             []              _ = throwError (UnsupportedFormat (show t))
parser t             (_        : xs) v = parser t xs v

-------------------------------------------------------------------------------
-- Failure responses.

failureWriter :: Rest m => Errors e -> Reason e -> m UTF8.ByteString
failureWriter es err =
  do formats <- accept
     fromMaybeT (printFallback formats) $
       msum (  (tryPrint err                     es    <$> (formats ++ [XmlFormat]))
            ++ (tryPrint (fallbackError formats) [NoE] <$> formats                 )
            )
  where
    tryPrint :: Rest m => Reason e -> Errors e -> Format -> MaybeT m UTF8.ByteString
    tryPrint e (JsonE   : _ ) JsonFormat = printError JsonFormat (toRespCode e) (encode e)
    tryPrint e (XmlE    : _ ) XmlFormat  = printError XmlFormat  (toRespCode e) (toXML  e)
    tryPrint e (NoE     : _ ) JsonFormat = printError JsonFormat (toRespCode e) (encode e)
    tryPrint e (NoE     : _ ) XmlFormat  = printError XmlFormat  (toRespCode e) (toXML  e)
    tryPrint e (_       : xs) t          = tryPrint e xs t
    tryPrint _ []             _          = mzero

    printError f cd x =
      do contentType f
         setResponseCode cd
         return (UTF8.fromString x)

    printFallback fs = printError XmlFormat (toRespCode (fallbackError fs)) (toXML $ fallbackError fs)

    fallbackError :: [Format] -> Reason_
    fallbackError fs = OutputError (UnsupportedFormat $ intercalate "," $ map formatCT fs)

    formatCT v =
      case v of
        XmlFormat    -> "xml"
        JsonFormat   -> "json"
        StringFormat -> "text/plain"
        FileFormat   -> "application/octet-stream"
        NoFormat     -> "any"

    fromMaybeT def = runMaybeT >=> maybe def return

    toRespCode e =
      case e of
        NotFound                               -> 404
        UnsupportedRoute                       -> 404
        UnsupportedMethod                      -> 404
        UnsupportedVersion                     -> 404
        UnacceptedFormat                       -> 422
        NotAllowed                             -> 403
        AuthenticationFailed                   -> 401
        Busy                                   -> 503
        Gone                                   -> 410
        OutputError (UnsupportedFormat _)      -> 406
        InputError  _                          -> 400
        OutputError _                          -> 500
        IdentError  _                          -> 400
        HeaderError _                          -> 400
        ParamError  _                          -> 400
        CustomReason (DomainReason toRespCd c) -> toRespCd c

-------------------------------------------------------------------------------
-- Printing the output resource.

contentType :: Rest m => Format -> m ()
contentType c = setHeader "Content-Type" $
  case c of
    JsonFormat -> "application/json; charset=UTF-8"
    XmlFormat  -> "application/xml; charset=UTF-8"
    _          -> "text/plain; charset=UTF-8"

validator :: forall v m e. Rest m => Outputs v -> ErrorT (Reason e) m ()
validator outputs = lift accept >>= \formats -> OutputError `mapE`
   (msum (try outputs <$> formats) <|> throwError (UnsupportedFormat (show formats)))

  where
    try :: Outputs v -> Format -> ErrorT DataError m ()
    try (XmlO    : _ ) XmlFormat    = return ()
    try (RawXmlO : _ ) XmlFormat    = return ()
    try (NoO     : _ ) NoFormat     = return ()
    try (NoO     : _ ) XmlFormat    = return ()
    try (NoO     : _ ) JsonFormat   = return ()
    try (NoO     : _ ) StringFormat = return ()
    try (JsonO   : _ ) JsonFormat   = return ()
    try (StringO : _ ) StringFormat = return ()
    try (FileO   : _ ) FileFormat   = return ()
    try []             t            = throwError (UnsupportedFormat (show t))
    try (_       : xs) t            = try xs t

outputWriter :: forall v m e. Rest m => Outputs v -> v -> ErrorT (Reason e) m UTF8.ByteString
outputWriter outputs v = lift accept >>= \formats -> OutputError `mapE`
  (msum (try outputs <$> formats) <|> throwError (UnsupportedFormat (show formats)))

  where
    try :: Outputs v -> Format -> ErrorT DataError m UTF8.ByteString
    try (XmlO    : _ ) XmlFormat    = contentType XmlFormat    >> ok (UTF8.fromString (toXML v))
    try (RawXmlO : _ ) XmlFormat    = contentType XmlFormat    >> ok v
    try (NoO     : _ ) NoFormat     = contentType NoFormat     >> ok ""
    try (NoO     : _ ) XmlFormat    = contentType NoFormat     >> ok "<done/>"
    try (NoO     : _ ) JsonFormat   = contentType NoFormat     >> ok "{}"
    try (NoO     : _ ) StringFormat = contentType NoFormat     >> ok "done"
    try (JsonO   : _ ) JsonFormat   = contentType JsonFormat   >> ok (UTF8.fromString (encode v))
    try (StringO : _ ) StringFormat = contentType StringFormat >> ok (UTF8.fromString v)
    try (FileO   : _ ) FileFormat   = do mime <- fromMaybe "application/octet-stream" <$> lookupMimeType (map toLower (snd v))
                                         setHeader "Content-Type" mime
                                         setHeader "Cache-Control" "private, max-age=604800"
                                         ok (fst v)
    try []             t            = throwError (UnsupportedFormat (show t))
    try (_       : xs) t            = try xs t
    ok r = setResponseCode 200 >> return r

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

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    txt "*"     = [XmlFormat, JsonFormat, StringFormat]
    txt "json"  = [JsonFormat]
    txt "xml"   = [XmlFormat]
    txt "plain" = [StringFormat]
    txt _       = []
    app "*"     = [XmlFormat, JsonFormat]
    app "xml"   = [XmlFormat]
    app "json"  = [JsonFormat]
    app _       = []
    img _       = [FileFormat]
