{-# LANGUAGE
    RankNTypes
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , GADTs
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  #-}
module Rest.Driver.Happstack (apiToHandler, apiToHandler') where

import Control.Applicative
import Control.Concurrent (readMVar)
import Control.Monad.Error
import Data.ByteString.Char8 (unpack)
import Data.Char (isSpace, toLower)
import Data.List
import Data.List.Split
import Data.Maybe
import Happstack.Server hiding (secure, Input, body, validator, Errors)
import Safe
import Text.JSON
import Text.Xml.Pickle
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Map                  as M

import Rest.Action
import Rest.Container
import Rest.Error
import Rest.Resource

type Run m n = forall a. m a -> n a

class    ( Functor m
         , Applicative m
         , Alternative m
         , Monad m
         , MonadPlus m
         , MonadIO m
         , ServerMonad m
         , HasRqData m
         , FilterMonad Response m
         ) => M m

instance ( Functor m
         , Applicative m
         , Alternative m
         , Monad m
         , MonadPlus m
         , MonadIO m
         , ServerMonad m
         , HasRqData m
         , FilterMonad Response m
         ) => M m

apiToHandler :: M n => Api n -> n Response
apiToHandler = versionHandler id

apiToHandler' :: (Monad m, M n) => Run m n -> Api m -> n Response
apiToHandler' = versionHandler

versionHandler :: (M n, Monad m) => Run m n -> Api m -> n Response
versionHandler run versions = path $ \i ->
  case i `lookupVersion` versions of
    Just (Some1 h) -> routerToHandler run h
    _              -> failureWriter [NoE] UnsupportedVersion

routerToHandler :: (M n, Monad m) => Run m n -> Router m s -> n Response
routerToHandler run (Embed res subs) =
  (if null (identifier res) then id else dir (identifier res)) $
    msum [ dispatch run res subs
         , multiHandlers run res
         , failureWriter [NoE] UnsupportedAction
         ]

dispatch :: (M n, Monad s, Monad m) => Run m n -> Resource m s a -> [Some1 (Router s)] -> n Response
dispatch run res subs =

  case singleUpdate res of
       []        -> mzero
       [("", z)] -> method PUT >> update' z
       zs        -> method PUT >> msum (updateDir <$> zs)

  <|>

  case singleUpdateBy res of
       []        -> mzero
       [("", z)] -> method PUT >> updateBy z
       zs        -> method PUT >> msum (updateByDir <$> zs)

  <|>

  case ( singleGet    res
       , singleGetBy  res
       , multiGetBy   res
       , multiActions res
       )

    of ([],        [],        [],        []) -> continue (error "no value for phony resource")
       ([("", x)], _,         _,         _ ) -> fetch     x
       (_,         [("", y)], _,         _ ) -> fetchBy   y
       (_,         _,         [("", m)], _ ) -> fetchesBy m
       (xs,        ys,        ms,        as) -> msum [ method POST >> actionHandlers run as
                                                     , msum (fetchDir     <$> xs)
                                                     , msum (fetchByDir   <$> ys)
                                                     , msum (fetchesByDir <$> ms)
                                                     ]

  where update'           act  =                                     updater    "" act
        updateDir    (d,  act) = dir d             (                 updater    "" act)
        updateBy          act  =             path  (\i -> nullDir >> updater     i act)
        updateByDir  (d,  act) = dir d     ( path  (\i -> nullDir >> updater     i act) <|> (nullDir >> updates run act))
        fetch             act  =                   (      nullDir >> fetcher    "" act)
        fetchBy           act  =             path  (\i ->            fetcher     i act)
        fetchDir     (d,  act) =  dir d            (                 fetcher    "" act)
        fetchByDir   (d,  act) = (dir d . optPath) (\i ->            fetcher     i act)
        fetchesBy         act  =             path  (\i -> nullDir >> listing run i act)
        fetchesByDir (d,  act) = (dir d . optPath) (\i -> nullDir >> listing run i act)

        optPath c = (nullDir >> c "") <|> path c

        updater i (Action act) = respond run i Nothing act

        fetcher i act =
          do values <- runErrorT (fetchValue run act i)
             case values of
               Left (SomeError es err) -> failureWriter es err
               Right v  -> singleHandlers run res act v <|> continue v

        continue v = msum ((\(Some1 r) -> routerToHandler (run . enter res v) r) <$> subs)

updates :: forall m n. M n => Run m n -> Action m -> n Response
updates run = (\(Action a) -> one a)
  where one :: Handler m a -> n Response
        one (Handler (i, h, p, j, o, es) prep act _) =
          do formats <- accept
             res <- runErrorT $
               do validator formats o
                  hs <- HeaderError `mapE` headers h
                  ps <- ParamError  `mapE` parameters p
                  Env _ _ _ (Map vs) <- fetchInputs "" (NoId, NoHeader, NoParam, mappingI j, o, es)
                  bs <- lift $ forM vs $ \(Key k, v) -> runErrorT $
                    do is <- IdentError `mapE` identifiers i k
                       mapErrorT run (act (Env is hs ps v)) >>= mapErrorT run . prep
                  let mapping = Map (map fst vs `zip` map eitherToStatus bs)
                  outputWriter formats ((mappingO . flip statusO (reasonE es)) o) mapping
             either (failureWriter es) return res

multiHandlers :: M n => Run m n -> Resource m s a -> n Response
multiHandlers run res = msum
  [ methodM GET  >> listing run "" `whenSupported` multiGet res
  , methodM POST >> case singleCreate res of
                      Just (Action a) -> respond run "" Nothing a
                      Nothing         -> failureWriter [NoE] UnsupportedAction
  ]

listing :: M n => Run m n -> String -> Handler m [a] -> n Response
listing run idnt h@(Handler (i, hs, p, j, o, es) prep act sec) =
  case o of
    [RawXmlO] -> respond run idnt Nothing h -- Bit of a Hack.
    _         ->
      do pars <- runErrorT (ParamError `mapE` parameters range)
         case pars of
           Left e       -> failureWriter es e
           Right (f, c) ->
             do let prep' xs = List f (min c (length xs)) `liftM` prep (take c xs)
                    handler = Handler (i, hs, p, j, listO o, es) prep' act sec
                respond run idnt Nothing handler

singleHandlers :: M n => Run m n -> Resource m s a -> Handler m a -> a -> n Response
singleHandlers run res act v = msum
  [ method   GET    >> actionHandlers (run . enter res v) (singleSelects res)
  , method   POST   >> actionHandlers (run . enter res v) (singleActions res)
  , methodM  GET    >> respond run "" (Just v) act
  , methodM  DELETE >> maybeAction (run . enter res v) "" (singleDelete res)
  ]

actionHandlers :: M n => Run m n -> [(String, Action m)] -> n Response
actionHandlers run = msum . map (\(d, Action act) -> dir d (respond run d Nothing act))

-------------------------------------------------------------------------------
-- Running handlers as a ServerPartDB.

whenSupported :: M n => (a -> n Response) -> Maybe a -> n Response
whenSupported f = failureWriter [NoE] UnsupportedAction `maybe` f

maybeAction :: M n => Run m n -> String -> Maybe (Action m) -> n Response
maybeAction _   _ Nothing             = failureWriter [NoE] UnsupportedAction
maybeAction run i (Just (Action act)) = respond run i Nothing act

-------------------------------------------------------------------------------
-- Generating responses.

respond :: M n => Run m n -> String -> Maybe a -> Handler m a -> n Response
respond run idnt w (Handler d@(_, _, _, _, o, es) prep act _) =
  do formats <- accept
     res <- runErrorT $
       do validator formats o
          v <- case w of
            Nothing -> fetchInputs idnt d >>= mapErrorT run . act
            Just v  -> return v
          mapErrorT run (prep v) >>= outputWriter formats o
     either (failureWriter es) return res

requestBody :: M m => m B.ByteString
requestBody =
  do rq <- askRq
     body <- liftIO (readMVar (rqBody rq))
     return (unBody body)

-------------------------------------------------------------------------------
-- Fetching the input resource.

fetchValue :: M n => Run m n -> Handler m a -> String -> ErrorT SomeError n a
fetchValue run (Handler (idents, hs, params, inputs, _, es) _ act _) inp = mapE (SomeError es) $
  do i <- IdentError  `mapE` identifiers idents inp
     h <- HeaderError `mapE` headers hs
     p <- ParamError  `mapE` parameters params
     j <- InputError  `mapE` parser NoFormat inputs B.empty
     mapErrorT run (act (Env i h p j))

fetchInputs :: M m => String -> Dict i h p j o e -> ErrorT (Reason e) m (Env i h p j)
fetchInputs idnt (idents, hs, params, inputs, _, _) =
  do bs <- lift requestBody
     ct <- parseContentType

     i <- IdentError  `mapE` identifiers idents idnt
     h <- HeaderError `mapE` headers hs
     p <- ParamError  `mapE` parameters params
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
     return (Env i h p j)

parseContentType :: ServerMonad m => m (Maybe Format)
parseContentType =
  do ct <- (maybe "" unpack . getHeader "Content-Type") `liftM` askRq
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

headers :: (ServerMonad m, Functor m, Monad m) => Header h -> ErrorT DataError m h
headers NoHeader      = return ()
headers (Header xs h) = mapM findHeader xs >>= either throwError return . h
  where findHeader x = askRq >>= return . fmap unpack . getHeader x

parameters :: (HasRqData m, Alternative m, Functor m, Monad m) => Param p -> ErrorT DataError m p
parameters NoParam      = return ()
parameters (Param xs p) = mapM (lift . findParam) xs >>= either throwError return . p
  where findParam x = Just <$> look x <|> return Nothing

identifiers :: Monad m => Ident i -> String -> ErrorT DataError m i
identifiers (NoId    ) _ = return ()
identifiers (ReadId  ) v = (throwError (ParseError "Read") `maybe` return) (readMay v)
identifiers (StringId) v = return v

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
-- Printing the output resource.

contentType :: M m => Format -> m ()
contentType c = setHeaderM "Content-Type" $
  case c of
    JsonFormat -> "application/json; charset=UTF-8"
    XmlFormat  -> "application/xml; charset=UTF-8"
    _          -> "text/plain; charset=UTF-8"

validator :: forall v m e. M m => [Format] -> Outputs v -> ErrorT (Reason e) m ()
validator formats outputs = OutputError `mapE`
   (msum (try outputs <$> formats) <|> throwError (UnsupportedFormat (show formats)))

  where
    try :: Monad m => Outputs v -> Format -> ErrorT DataError m ()
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

outputWriter :: forall v m e. M m => [Format] -> Outputs v -> v -> ErrorT (Reason e) m Response
outputWriter formats outputs v = OutputError `mapE`
  (msum (try outputs <$> formats) <|> throwError (UnsupportedFormat (show formats)))

  where
    try :: Outputs v -> Format -> ErrorT DataError m Response
    try (XmlO    : _ ) XmlFormat    = lift (contentType XmlFormat    >> ok (toResponse (toXML v)))
    try (RawXmlO : _ ) XmlFormat    = lift (contentType XmlFormat    >> return (resultBS 200 v))
    try (NoO     : _ ) NoFormat     = lift (contentType NoFormat     >> ok (toResponse ("" :: String)))
    try (NoO     : _ ) XmlFormat    = lift (contentType NoFormat     >> ok (toResponse ("<done/>" :: String)))
    try (NoO     : _ ) JsonFormat   = lift (contentType NoFormat     >> ok (toResponse ("{}" :: String)))
    try (NoO     : _ ) StringFormat = lift (contentType NoFormat     >> ok (toResponse ("done" :: String)))
    try (JsonO   : _ ) JsonFormat   = lift (contentType JsonFormat   >> ok (toResponse (encode v)))
    try (StringO : _ ) StringFormat = lift (contentType StringFormat >> ok (toResponse v))
    try (FileO   : _ ) FileFormat   = lift $ do let mime = fromMaybe "application/octet-stream" (M.lookup (map toLower (snd v)) mimeTypes)
                                                setHeaderM "Content-Type" mime
                                                setHeaderM "Cache-Control" "private, max-age=86400"
                                                return (resultBS 200 (fst v))
    try []             t            = throwError (UnsupportedFormat (show t))
    try (_       : xs) t            = try xs t

accept :: M m => m [Format]
accept =
  do rq <- askRq
     ct <- parseContentType
     ty <- look "type" <|> return ""
     let fromQuery =
           case ty of
             "json" -> [JsonFormat]
             "xml"  -> [XmlFormat]
             _      -> []
         fromAccept = maybe (allFormats ct) (splitter ct . unpack) (getHeader "Accept" rq)
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

-------------------------------------------------------------------------------
-- Failure responses.

failureWriter :: M m => Errors e -> Reason e -> m Response
failureWriter es e =
  do formats <- (++ [XmlFormat]) <$> accept
     msum (errorPrinter <$> formats)

  where
{-   errorPrinter JsonFormat = contentType JsonFormat >> out (encode e)
    errorPrinter XmlFormat  = contentType XmlFormat  >> out (toXML e)
    errorPrinter _          = contentType XmlFormat  >> out (toXML e)
-}
    errorPrinter f = case tryPrint f of
                        []      -> failureWriter [NoE] (OutputError (UnsupportedFormat $ formatCT f))
                        (x : _) -> contentType f >> out x

    formatCT :: Format -> String
    formatCT v =
      case v of
        XmlFormat    -> "xml"
        JsonFormat   -> "json"
        StringFormat -> "text/plain"
        FileFormat   -> "application/octet-stream"
        NoFormat     -> "any"

    -- | Try to print the error in the same format as requested, otherwise just print the error
    tryPrint JsonFormat   = concatMap (\v -> case v of { JsonE -> [encode e]; NoE -> [encode e]; _ -> []}) es
    tryPrint XmlFormat    = concatMap (\v -> case v of { XmlE -> [toXML e]; NoE -> [toXML e]; _ -> []}) es
    tryPrint _            = tryPrint XmlFormat ++ tryPrint JsonFormat

    out = case e of
            NotFound                           -> notFound
            PreparationFailed                  -> internalServerError
            UnsupportedResource                -> notFound
            UnsupportedAction                  -> notFound
            UnsupportedVersion                 -> notFound
            UnacceptedFormat                   -> resp 422
            NotAllowed                         -> forbidden
            AuthenticationFailed               -> unauthorized
            Busy                               -> resp 503
            OutputError (UnsupportedFormat _)  -> resp 406
            InputError  _                      -> badRequest
            OutputError _                      -> internalServerError
            IdentError  _                      -> badRequest
            HeaderError _                      -> badRequest
            ParamError  _                      -> badRequest
            CustomReason (DomainReason toRespCode c) -> resp (toRespCode c)
            Unknown     _                      -> internalServerError
          . toResponse

