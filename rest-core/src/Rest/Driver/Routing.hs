{-# LANGUAGE ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , RankNTypes
           , NamedFieldPuns
           , FlexibleContexts
           , StandaloneDeriving
           , GADTs
           #-}
module Rest.Driver.Routing where

import Control.Applicative
import Control.Error.Util
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT, MonadState)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Safe
import qualified Control.Monad.State   as State
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.UTF8  as UTF8
import qualified Data.Label.Total      as L

import Network.URI.Encode (decodeByteString)
import Rest.Container
import Rest.Dictionary
import Rest.Error
import Rest.Handler (ListHandler, Handler, GenHandler (..), Env (..), range)
import Rest.Resource (Some1(..))
import qualified Rest.Resource as Rest
import qualified Rest.Schema   as Rest

import Rest.Driver.Types

data Method = GET | PUT | POST | DELETE | Unknown String
  deriving (Show, Eq)
type Uri = ByteString
type UriParts = [String]

apiError :: (MonadError (Reason e) m) => Reason e -> m a
apiError = throwError

newtype Router a =
  Router { unRouter :: ReaderT Method (StateT UriParts (EitherT Reason_ Identity)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Method
           , MonadState UriParts
           , MonadError Reason_
           )

runRouter :: Method -> UriParts -> Router (RunnableHandler m) -> Either Reason_ (RunnableHandler m)
runRouter method uri = runIdentity
                     . runEitherT
                     . flip evalStateT uri
                     . flip runReaderT method
                     . unRouter

route :: Method -> UriParts -> Rest.Api m -> Either Reason_ (RunnableHandler m)
route method uri api = runRouter method uri $
  do versionStr <- popSegment
     case versionStr `Rest.lookupVersion` api of
          Just (Some1 router) -> routeRoot router
          _                   -> apiError UnsupportedVersion

routeRoot :: Rest.Router m s -> Router (RunnableHandler m)
routeRoot router@(Rest.Embed resource _) = do
  routeName (Rest.name resource)
  routeRouter router

routeRouter :: Rest.Router m s -> Router (RunnableHandler m)
routeRouter (Rest.Embed resource@(Rest.Resource { Rest.schema }) subRouters) =
  case schema of
    (Rest.Schema mToplevel step) -> maybe (apiError UnsupportedRoute) return =<< runMaybeT
       (  routeToplevel resource subRouters mToplevel
      <|> routeCreate resource
      <|> lift (routeStep resource subRouters step)
       )

routeToplevel :: Rest.Resource m s sid mid aid
              -> [Some1 (Rest.Router s)]
              -> Maybe (Rest.Cardinality sid mid)
              -> MaybeT Router (RunnableHandler m)
routeToplevel resource@(Rest.Resource { Rest.list }) subRouters mToplevel =
  hoistMaybe mToplevel >>= \toplevel ->
  case toplevel of
    Rest.Single sid -> lift $ withSubresource sid resource subRouters
    Rest.Many   mid ->
      do guardNullPath
         guardMethod GET
         return (RunnableHandler id (mkListHandler (list mid)))

routeCreate :: Rest.Resource m s sid mid aid -> MaybeT Router (RunnableHandler m)
routeCreate (Rest.Resource { Rest.create }) = guardNullPath >> guardMethod POST >>
  maybe (apiError UnsupportedRoute) (return . RunnableHandler id) create

routeStep :: Rest.Resource m s sid mid aid
          -> [Some1 (Rest.Router s)]
          -> Rest.Step sid mid aid
          -> Router (RunnableHandler m)
routeStep resource subRouters step =
  case step of
    Rest.Named ns -> popSegment >>= \seg ->
      case lookup seg ns of
        Nothing -> apiError UnsupportedRoute
        Just h  -> routeNamed resource subRouters h
    Rest.Unnamed h -> routeUnnamed resource subRouters h

routeNamed :: Rest.Resource m s sid mid aid
           -> [Some1 (Rest.Router s)]
           -> Rest.Endpoint sid mid aid
           -> Router (RunnableHandler m)
routeNamed resource@(Rest.Resource { Rest.list, Rest.statics }) subRouters h =
  case h of
    Left aid -> noRestPath >> hasMethod POST >> return (RunnableHandler id (statics aid))
    Right (Rest.Single getter) -> routeGetter getter resource subRouters
    Right (Rest.Many   getter) -> routeListGetter getter list

routeUnnamed :: Rest.Resource m s sid mid aid
             -> [Some1 (Rest.Router s)]
             -> Rest.Cardinality (Rest.Id sid) (Rest.Id mid)
             -> Router (RunnableHandler m)
routeUnnamed resource@(Rest.Resource { Rest.list }) subRouters cardinality = popSegment >>= \seg ->
  case cardinality of
    Rest.Single sBy -> parseIdent sBy seg >>= \sid -> withSubresource sid resource subRouters
    Rest.Many   mBy -> parseIdent mBy seg >>= \mid ->
      do noRestPath
         hasMethod GET
         return (RunnableHandler id (mkListHandler (list mid)))

routeGetter :: Rest.Getter sid
            -> Rest.Resource m s sid mid aid
            -> [Some1 (Rest.Router s)]
            -> Router (RunnableHandler m)
routeGetter getter resource subRouters =
  case getter of
    Rest.Singleton sid -> getOrDeep sid
    Rest.By        sBy -> withSegment (multiPut resource sBy) $ \seg ->
                            parseIdent sBy seg >>= getOrDeep
  where
    getOrDeep sid = withSubresource sid resource subRouters

multiPut :: Rest.Resource m s sid mid aid -> Rest.Id sid -> Router (RunnableHandler m)
multiPut (Rest.Resource { Rest.update, Rest.enter }) sBy = hasMethod PUT >>
  case update of
    Just updateH -> return (RunnableHandler id (mkMultiPutHandler sBy enter updateH))
    Nothing      -> apiError UnsupportedRoute

routeListGetter :: Monad m
                => Rest.Getter mid
                -> (mid -> ListHandler m)
                -> Router (RunnableHandler m)
routeListGetter getter list = hasMethod GET >>
  case getter of
    Rest.Singleton mid -> noRestPath >> return (RunnableHandler id (mkListHandler (list mid)))
    Rest.By        mBy ->
      do seg <- popSegment
         mid <- parseIdent mBy seg
         noRestPath
         return (RunnableHandler id (mkListHandler (list mid)))

withSubresource :: sid
                -> Rest.Resource m s sid mid aid
                -> [Some1 (Rest.Router s)]
                -> Router (RunnableHandler m)
withSubresource sid resource@(Rest.Resource { Rest.enter, Rest.selects, Rest.actions }) subRouters =
  withSegment (routeSingle sid resource) $ \seg ->
  case lookup seg selects of
    Just select -> noRestPath >> hasMethod GET >> return (RunnableHandler (enter sid) select)
    Nothing -> case lookup seg actions of
      Just action -> noRestPath >> hasMethod POST >> return (RunnableHandler (enter sid) action)
      Nothing ->
        case lookupRouter seg subRouters of
          Just (Some1 subRouter) -> do
            (RunnableHandler subRun subHandler) <- routeRouter subRouter
            return (RunnableHandler (enter sid . subRun) subHandler)
          Nothing -> apiError UnsupportedRoute

routeSingle :: sid -> Rest.Resource m s sid mid aid -> Router (RunnableHandler m)
routeSingle sid (Rest.Resource { Rest.enter, Rest.get, Rest.update, Rest.remove }) =
  ask >>= \method ->
  case method of
    GET    -> handleOrNotFound get
    PUT    -> handleOrNotFound update
    DELETE -> handleOrNotFound remove
    _      -> apiError UnsupportedMethod
  where
    handleOrNotFound = maybe (apiError UnsupportedRoute) (return . RunnableHandler (enter sid))

routeName :: String -> Router ()
routeName ident = when (not . null $ ident) $
  do identStr <- popSegment
     when (identStr /= ident) $
       apiError UnsupportedRoute

lookupRouter :: String -> [Some1 (Rest.Router s)] -> Maybe (Some1 (Rest.Router s))
lookupRouter _    [] = Nothing
lookupRouter name (Some1 router@(Rest.Embed resource _) : routers)
   =  (guard (Rest.name resource == name) >> return (Some1 router))
  <|> lookupRouter name routers

parseIdent :: MonadError (Reason e) m => Rest.Id id -> String -> m id
parseIdent (Rest.Id StringId byF) seg = return (byF seg)
parseIdent (Rest.Id ReadId   byF) seg =
  case readMay seg of
    Nothing  -> throwError (IdentError (ParseError $ "Failed to parse " ++ seg))
    Just sid -> return (byF sid)

splitUri :: Uri -> UriParts
splitUri = filter (/= "") . map (UTF8.toString . decodeByteString) . Char.split '/'

popSegment :: Router String
popSegment =
  do uriParts <- State.get
     case uriParts of
       []      -> apiError UnsupportedRoute
       (hd:tl) -> do
         State.put tl
         return hd

withSegment :: Router a -> (String -> Router a) -> Router a
withSegment noSeg withSeg =
  do uriParts <- State.get
     case uriParts of
       [] -> noSeg
       (hd:tl) -> do
         State.put tl
         withSeg hd

noRestPath :: Router ()
noRestPath =
  do uriParts <- State.get
     unless (null uriParts) $
       apiError UnsupportedRoute

guardNullPath :: (MonadPlus m, MonadState UriParts m) => m ()
guardNullPath = State.get >>= guard . null

hasMethod :: Method -> Router ()
hasMethod wantedMethod = ask >>= \method ->
  if method == wantedMethod
  then return ()
  else apiError UnsupportedMethod

guardMethod :: (MonadPlus m, MonadReader Method m) => Method -> m ()
guardMethod method = ask >>= guard . (== method)

mkListHandler :: Monad m => ListHandler m -> Handler m
mkListHandler (GenHandler dict act sec) =
  GenHandler (addPar range . L.modify outputs listO $ dict) (mkListAction act) sec

mkMultiPutHandler :: Monad m => Rest.Id id -> (id -> Run s m) -> Handler s -> Handler m
mkMultiPutHandler sBy run (GenHandler dict act sec) = GenHandler newDict newAct sec
  where
    newErrDict = L.modify errors reasonE dict
    newDict = L.modify inputs mappingI
            . L.modify outputs (mappingO . statusO (L.get errors newErrDict))
            $ newErrDict
    newAct (Env hs ps (StringMap vs)) =
      do bs <- lift $ forM vs $ \(k, v) -> runErrorT $
           do i <- parseIdent sBy k
              mapErrorT (run i) (act (Env hs ps v))
         return (StringMap (zipWith (\(k, _) b -> (k, eitherToStatus b)) vs bs))

mkListAction :: Monad m
            => (Env h p i -> ErrorT (Reason e) m [a])
            -> Env h ((Int, Int), p) i
            -> ErrorT (Reason e) m (List a)
mkListAction act (Env h ((f, c), p) i) = do
  xs <- act (Env h p i)
  return (List f (min c (length xs)) (take c xs))
