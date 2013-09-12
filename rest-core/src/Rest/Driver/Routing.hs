{-# LANGUAGE ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , RankNTypes
           , NamedFieldPuns
           , FlexibleContexts
           , StandaloneDeriving
           #-}
module Rest.Driver.Routing where

import Control.Applicative
import Control.Error.Util
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT, MonadState)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.UTF8  as UTF8
import qualified Control.Monad.State   as State

import Network.URI.Encode (decodeByteString)
import Rest.Action (ListHandler)
import Rest.Dictionary
import Rest.Error
import Rest.Resource (Some1(..))
import qualified Rest.Resource as Rest

import Rest.Driver.Types

data Method = GET | PUT | POST | DELETE
  deriving (Show, Eq, Bounded, Enum)
type Uri = ByteString
type UriParts = [String]
data ApiError = forall e. Show e => ApiError (Errors e) (Reason e)
deriving instance Show ApiError

apiError :: (Show e, MonadError ApiError m) => Errors e -> Reason e -> m a
apiError es r = throwError (ApiError es r)

newtype Router a = Router { unRouter :: ReaderT Method (StateT UriParts (EitherT ApiError Identity)) a }
  deriving (Functor, Applicative, Monad, MonadReader Method, MonadState UriParts, MonadError ApiError)

runRouter :: Method -> Uri -> Router (RunnableHandler m) -> Either ApiError (RunnableHandler m)
runRouter method uri router = runIdentity . runEitherT $ evalStateT (runReaderT (unRouter router) method) (splitUri uri)

route :: Method -> Uri -> Rest.Api m -> Either ApiError (RunnableHandler m)
route method uri api = runRouter method uri $
  do versionStr <- popSegment
     case versionStr `Rest.lookupVersion` api of
          Just (Some1 router) -> routeRoot router
          _                   -> apiError [NoE] UnsupportedVersion

routeRoot :: Rest.Router m s -> Router (RunnableHandler m)
routeRoot router@(Rest.Embed resource _) = do
  routeName (Rest.name resource)
  routeRouter router

routeRouter :: Rest.Router m s -> Router (RunnableHandler m)
routeRouter (Rest.Embed resource@(Rest.Resource { Rest.schema }) subRouters) =
  case schema of
    (Rest.Schema mToplevel step) -> maybe (apiError [NoE] UnsupportedRoute) return =<< runMaybeT
       (  routeToplevel resource subRouters mToplevel
      <|> routeCreate resource
      <|> lift (routeStep resource subRouters step)
       )

routeToplevel :: Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Maybe (Rest.Cardinality sid mid) -> MaybeT Router (RunnableHandler m)
routeToplevel resource@(Rest.Resource { Rest.list }) subRouters mToplevel = hoistMaybe mToplevel >>= \toplevel ->
  case toplevel of
    Rest.Single sid -> lift $ withSubresource sid resource subRouters
    Rest.Many   mid -> guardNullPath >> guardMethod GET >> return (RunnableHandler mid id list)

routeCreate :: Rest.Resource m s sid mid aid -> MaybeT Router (RunnableHandler m)
routeCreate (Rest.Resource { Rest.create }) = guardNullPath >> guardMethod POST >>
  maybe (apiError [NoE] UnsupportedRoute) (return . RunnableHandler () id) create

routeStep :: Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Rest.Step sid mid aid -> Router (RunnableHandler m)
routeStep resource subRouters step =
  case step of
    Rest.Named ns -> popSegment >>= \seg ->
      case lookup seg ns of
        Nothing -> apiError [NoE] UnsupportedRoute
        Just h  -> routeNamed resource subRouters h
    Rest.Unnamed h -> routeUnnamed resource subRouters h

routeNamed :: Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Either aid (Rest.Cardinality (Rest.Getter sid) (Rest.Getter mid)) -> Router (RunnableHandler m)
routeNamed resource@(Rest.Resource { Rest.list, Rest.statics }) subRouters h =
  case h of
    Left aid -> noRestPath >> hasMethod POST >> return (RunnableHandler aid id statics)
    Right (Rest.Single getter) -> routeGetter getter resource subRouters
    Right (Rest.Many   getter) -> routeListGetter getter list

routeUnnamed :: Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Rest.Cardinality (String -> sid) (String -> mid) -> Router (RunnableHandler m)
routeUnnamed resource@(Rest.Resource { Rest.list }) subRouters cardinality = popSegment >>= \seg ->
  case cardinality of
    Rest.Single sBy -> withSubresource (sBy seg) resource subRouters
    Rest.Many   mBy -> noRestPath >> hasMethod GET >> return (RunnableHandler (mBy seg) id list)

routeGetter :: Rest.Getter sid -> Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Router (RunnableHandler m)
routeGetter getter resource subRouters =
  case getter of
    Rest.Singleton sid -> getOrDeep sid
    Rest.By        sBy -> popSegment >>= getOrDeep . sBy
  where
    getOrDeep sid = withSubresource sid resource subRouters

routeListGetter :: Rest.Getter mid -> ListHandler mid m -> Router (RunnableHandler m)
routeListGetter getter list = hasMethod GET >>
  case getter of
    Rest.Singleton mid -> noRestPath >> return (RunnableHandler mid id list)
    Rest.By        mBy -> popSegment >>= \seg -> noRestPath  >> return (RunnableHandler (mBy seg) id list)

withSubresource :: sid -> Rest.Resource m s sid mid aid -> [Some1 (Rest.Router s)] -> Router (RunnableHandler m)
withSubresource sid resource@(Rest.Resource { Rest.enter, Rest.selects, Rest.actions }) subRouters =  withSegment (routeSingle sid resource) $ \seg ->
  case lookup seg selects of
    Just select -> noRestPath >> hasMethod GET >> return (RunnableHandler sid id select)
    Nothing -> case lookup seg actions of
      Just action -> noRestPath >> hasMethod POST >> return (RunnableHandler sid id action)
      Nothing ->
        case lookupRouter seg subRouters of
          Just (Some1 subRouter) -> do
            (RunnableHandler subId subRun subHandler) <- routeRouter subRouter
            return (RunnableHandler subId (enter sid . subRun) subHandler)
          Nothing -> apiError [NoE] UnsupportedRoute

routeSingle :: sid -> Rest.Resource m s sid mid aid -> Router (RunnableHandler m)
routeSingle sid (Rest.Resource { Rest.get, Rest.update, Rest.remove }) = ask >>= \method ->
  case method of
    GET    -> handleOrNotFound get
    PUT    -> handleOrNotFound update
    DELETE -> handleOrNotFound remove
    _      -> apiError [NoE] UnsupportedMethod
  where
    handleOrNotFound = maybe (apiError [NoE] UnsupportedRoute) (return . RunnableHandler sid id)

routeName :: String -> Router ()
routeName ident =
  do when (not . null $ ident) $ do
       identStr <- popSegment
       when (identStr /= ident) $
         apiError [NoE] UnsupportedRoute

lookupRouter :: String -> [Some1 (Rest.Router s)] -> Maybe (Some1 (Rest.Router s))
lookupRouter _    [] = Nothing
lookupRouter name (Some1 router@(Rest.Embed resource _) : routers)
   =  (guard (Rest.name resource == name) >> return (Some1 router))
  <|> lookupRouter name routers

splitUri :: Uri -> [String]
splitUri = filter (/= "") . map (UTF8.toString . decodeByteString) . Char.split '/'

popSegment :: Router String
popSegment =
  do uriParts <- State.get
     case uriParts of
       []      -> apiError [NoE] UnsupportedRoute
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
       apiError [NoE] UnsupportedRoute

guardNullPath :: (MonadPlus m, MonadState UriParts m) => m ()
guardNullPath = State.get >>= guard . null

hasMethod :: Method -> Router ()
hasMethod wantedMethod = ask >>= \method ->
  if method == wantedMethod
  then return ()
  else apiError [NoE] UnsupportedMethod

guardMethod :: (MonadPlus m, MonadReader Method m) => Method -> m ()
guardMethod method = ask >>= guard . (== method)
