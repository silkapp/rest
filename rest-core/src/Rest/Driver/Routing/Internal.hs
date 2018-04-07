{-# LANGUAGE
    CPP
  , ExistentialQuantification
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Rest.Driver.Routing.Internal where

import Prelude.Compat hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Error.Util
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.List.Split
import Network.Multipart (BodyPart (..), HeaderName (..))
import Safe
import qualified Control.Monad.State       as State
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.HashMap.Strict       as H
import qualified Data.Label.Total          as L

import Network.URI.Encode (decode)
import Rest.Api (Some1 (..))
import Rest.Container
import Rest.Dictionary
import Rest.Error
import Rest.Handler (Env (..), GenHandler (..), Handler, ListHandler, Range (..), mkInputHandler, range)
import Rest.Types.Container.Resource (Resource, Resources (..))
import qualified Rest.Api                      as Rest
import qualified Rest.Resource                 as Rest
import qualified Rest.Schema                   as Rest
import qualified Rest.StringMap.HashMap.Strict as StringHashMap
import qualified Rest.Types.Container.Resource as R

import Rest.Driver.Perform (failureWriter, writeResponse)
import Rest.Driver.RestM (runRestM)
import Rest.Driver.Types

import qualified Rest.Driver.RestM as Rest

{-# ANN module "HLint: ignore Reduce duplication" #-}

type UriParts = [String]

apiError :: (MonadError (Reason e) m) => Reason e -> m a
apiError = throwError

data RouterData m = RouterData { method :: Method, config :: Config m }

newtype Router m a =
  Router { unRouter :: ReaderT (RouterData m) (StateT UriParts (ExceptT Reason_ Identity)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (RouterData m)
           , MonadState UriParts
           , MonadError Reason_
           )

runRouter :: Config m -> Method -> UriParts -> Router m (RunnableHandler m) -> Either Reason_ (RunnableHandler m)
runRouter cfg mtd uri = runIdentity
                      . runExceptT
                      . flip evalStateT uri
                      . flip runReaderT (RouterData mtd cfg)
                      . unRouter

route :: (Applicative m, Monad m) => Maybe Method -> UriParts -> Rest.Api m -> Either Reason_ (RunnableHandler m)
route = routeWith defaultConfig

routeWith :: Config m -> Maybe Method -> UriParts -> Rest.Api m -> Either Reason_ (RunnableHandler m)
routeWith _    Nothing   _   _   = apiError UnsupportedMethod
routeWith cfg (Just mtd) uri api = runRouter cfg mtd uri $
  case api of
    Rest.Unversioned (Some1 router) -> routeRoot router
    Rest.Versioned   vrs            -> do
      versionStr <- popSegment
      case versionStr `Rest.lookupVersion` vrs of
          Just (Some1 router) -> routeRoot router
          _                   -> apiError UnsupportedVersion

routeRoot :: Rest.Router m s -> Router m (RunnableHandler m)
routeRoot router@(Rest.Embed resource _) = do
  routeName (Rest.name resource)
  fromMaybeT (routeRouter router) (routeMultiGet router)

routeMultiGet :: Rest.Router m s -> MaybeT (Router m) (RunnableHandler m)
routeMultiGet root@(Rest.Embed Rest.Resource{} _) =
  do guardNullPath
     guardMethod POST
     cfg <- asks config
     return (RunnableHandler id (mkMultiGetHandler cfg root))

routeRouter :: Rest.Router m s -> Router n (RunnableHandler m)
routeRouter (Rest.Embed resource@Rest.Resource { Rest.schema } subRouters) =
  case schema of
    (Rest.Schema mToplevel step) -> maybe (apiError UnsupportedRoute) return =<< runMaybeT
       (  routeToplevel resource subRouters mToplevel
      <|> routeCreate resource
      <|> lift (routeStep resource subRouters step)
       )

routeToplevel :: Rest.Resource m s sid mid aid
              -> [Some1 (Rest.Router s)]
              -> Maybe (Rest.Cardinality sid mid)
              -> MaybeT (Router n) (RunnableHandler m)
routeToplevel resource@Rest.Resource { Rest.list } subRouters mToplevel =
  hoistMaybe mToplevel >>= \toplevel ->
  case toplevel of
    Rest.Single sid -> lift $ withSubresource sid resource subRouters
    Rest.Many   mid ->
      do guardNullPath
         guardMethod GET
         lift $ routeListHandler (list mid)

routeCreate :: Rest.Resource m s sid mid aid -> MaybeT (Router n) (RunnableHandler m)
routeCreate Rest.Resource { Rest.create } = guardNullPath >> guardMethod POST >>
  maybe (apiError UnsupportedRoute) (return . RunnableHandler id) create

routeStep :: Rest.Resource m s sid mid aid
          -> [Some1 (Rest.Router s)]
          -> Rest.Step sid mid aid
          -> Router n (RunnableHandler m)
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
           -> Router n (RunnableHandler m)
routeNamed resource@Rest.Resource { Rest.list, Rest.statics } subRouters h =
  case h of
    Left aid -> noRestPath >> hasMethod POST >> return (RunnableHandler id (statics aid))
    Right (Rest.Single getter) -> routeGetter getter resource subRouters
    Right (Rest.Many   getter) -> routeListGetter getter list

routeUnnamed :: Rest.Resource m s sid mid aid
             -> [Some1 (Rest.Router s)]
             -> Rest.Cardinality (Rest.Id sid) (Rest.Id mid)
             -> Router n (RunnableHandler m)
routeUnnamed resource@Rest.Resource { Rest.list } subRouters cardinality =
  case cardinality of
    Rest.Single sBy -> withSegment (multi resource sBy) $
      parseIdent sBy >=> \sid -> withSubresource sid resource subRouters
    Rest.Many   mBy ->
      do seg <- popSegment
         mid <- parseIdent mBy seg
         noRestPath
         hasMethod GET
         routeListHandler (list mid)

routeGetter :: Rest.Getter sid
            -> Rest.Resource m s sid mid aid
            -> [Some1 (Rest.Router s)]
            -> Router n (RunnableHandler m)
routeGetter getter resource subRouters =
  case getter of
    Rest.Singleton sid -> getOrDeep sid
    Rest.By        sBy -> withSegment (multi resource sBy) $
                            parseIdent sBy >=> getOrDeep
  where
    getOrDeep sid = withSubresource sid resource subRouters

multi :: Rest.Resource m s sid mid aid -> Rest.Id sid -> Router n (RunnableHandler m)
multi Rest.Resource { Rest.update, Rest.remove, Rest.enter } sBy =
  asks method >>= \mtd ->
  case mtd of
    PUT    -> handleOrNotFound update
    DELETE -> handleOrNotFound remove
    _      -> apiError UnsupportedMethod
  where
    handleOrNotFound handler =
      maybe (apiError UnsupportedRoute)
            (return . RunnableHandler id)
            (handler >>= mkMultiHandler sBy enter)

routeListGetter :: Monad m
                => Rest.Getter mid
                -> (mid -> ListHandler m)
                -> Router n (RunnableHandler m)
routeListGetter getter list = hasMethod GET >>
  case getter of
    Rest.Singleton mid -> noRestPath >> routeListHandler (list mid)
    Rest.By        mBy ->
      do seg <- popSegment
         mid <- parseIdent mBy seg
         noRestPath
         routeListHandler (list mid)

withSubresource :: sid
                -> Rest.Resource m s sid mid aid
                -> [Some1 (Rest.Router s)]
                -> Router n (RunnableHandler m)
withSubresource sid resource@Rest.Resource { Rest.enter, Rest.selects, Rest.actions } subRouters =
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

routeSingle :: sid -> Rest.Resource m s sid mid aid -> Router n (RunnableHandler m)
routeSingle sid Rest.Resource { Rest.enter, Rest.get, Rest.update, Rest.remove } =
  asks method >>= \mtd ->
  case mtd of
    GET    -> handleOrNotFound get
    PUT    -> handleOrNotFound update
    DELETE -> handleOrNotFound remove
    _      -> apiError UnsupportedMethod
  where
    handleOrNotFound = maybe (apiError UnsupportedRoute) (return . RunnableHandler (enter sid))

routeName :: String -> Router n ()
routeName ident = unless (null ident) $
  do identStr <- popSegment
     when (identStr /= ident) $
       apiError UnsupportedRoute

routeListHandler :: Monad m => ListHandler m -> Router n (RunnableHandler m)
routeListHandler list =
      maybe (apiError UnsupportedRoute)
            (return . RunnableHandler id)
            (mkListHandler list)

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

splitUriString :: String -> UriParts
splitUriString = filter (/= "") . map decode . splitOn "/"

popSegment :: Router n String
popSegment =
  do uriParts <- State.get
     case uriParts of
       []      -> apiError UnsupportedRoute
       (hd:tl) -> do
         State.put tl
         return hd

withSegment :: Router n a -> (String -> Router n a) -> Router n a
withSegment noSeg withSeg =
  do uriParts <- State.get
     case uriParts of
       [] -> noSeg
       (hd:tl) -> do
         State.put tl
         withSeg hd

noRestPath :: Router n ()
noRestPath =
  do uriParts <- State.get
     unless (null uriParts) $
       apiError UnsupportedRoute

guardNullPath :: (MonadPlus m, MonadState UriParts m) => m ()
guardNullPath = State.get >>= guard . null

hasMethod :: Method -> Router n ()
hasMethod wantedMethod = asks method >>= \mtd ->
  unless (mtd == wantedMethod) $
    apiError UnsupportedMethod

guardMethod :: (MonadPlus m, MonadReader (RouterData c) m) => Method -> m ()
guardMethod mtd = asks method >>= guard . (== mtd)

mkListHandler :: Monad m => ListHandler m -> Maybe (Handler m)
mkListHandler (GenHandler dict act sec) =
  do newDict <- L.traverse outputs listO . addPar range $ dict
     return $ GenHandler newDict (mkListAction act) sec

mkListAction :: Monad m
            => (Env h p i -> ExceptT (Reason e) m [a])
            -> Env h (Range, p) i
            -> ExceptT (Reason e) m (List a)
mkListAction act (Env h (Range f c, p) i) = do
  xs <- act (Env h p i)
  return (List f (min c (length xs)) (take c xs))

mkMultiHandler :: Monad m => Rest.Id id -> (id -> Run s m) -> Handler s -> Maybe (Handler m)
mkMultiHandler sBy run (GenHandler dict act sec) = GenHandler <$> mNewDict <*> pure newAct <*> pure sec
  where
    newErrDict = L.modify errors reasonE dict
    mNewDict =  L.traverse inputs mappingI
            <=< L.traverse outputs (mappingO <=< statusO (L.get errors newErrDict))
             .  L.set errors defaultE
             $  dict
    newAct (Env hs ps vs) =
      do bs <- lift $ forM (StringHashMap.toList vs) $ \(k, v) -> runExceptT $
           do i <- parseIdent sBy k
              mapExceptT (run i) (act (Env hs ps v))
         return . StringHashMap.fromList $ zipWith (\(k, _) b -> (k, eitherToStatus b)) (StringHashMap.toList vs) bs

mkMultiGetHandler :: forall m s. (Applicative m, Monad m) => Config m -> Rest.Router m s -> Handler m
mkMultiGetHandler cfg root = mkInputHandler (xmlJsonI . multipartO) $ \(Resources rs) -> runMultiResources cfg cfg root rs

defaultRunMultiResources :: (Applicative m, Monad m) => Config m -> Rest.Router m s -> [Resource] -> ExceptT Reason_ m [BodyPart]
defaultRunMultiResources cfg root rs = lift $ forM rs (runResource cfg root)

defaultConfig :: (Applicative m, Monad m) => Config m
defaultConfig = Config { runMultiResources = defaultRunMultiResources }

runResource :: (Applicative m, Monad m) => Config m -> Rest.Router m s -> Resource -> m BodyPart
runResource cfg root res
  = fmap (uncurry mkBodyPart)
  . runRestM (toRestInput res)
  . either (failureWriter None) (writeResponse . mapHandler lift)
  $ routeResource cfg root res

routeResource :: Config m -> Rest.Router m s -> Resource -> Either Reason_ (RunnableHandler m)
routeResource cfg root res = runRouter cfg (R.method res) (splitUriString $ R.uri res) (routeRoot root)

toRestInput :: Resource -> Rest.RestInput
toRestInput r = Rest.emptyInput
  { Rest.headers    = H.map R.unValue . StringHashMap.toHashMap . R.headers    $ r
  , Rest.parameters = H.map R.unValue . StringHashMap.toHashMap . R.parameters $ r
  , Rest.body       = LUTF8.fromString (R.input r)
  , Rest.method     = Just (R.method r)
  , Rest.paths      = splitUriString (R.uri r)
  }

mkHeaders :: H.HashMap String d -> [(HeaderName, d)]
mkHeaders = map (first HeaderName) . H.toList

mkBodyPart :: LUTF8.ByteString -> Rest.RestOutput -> BodyPart
mkBodyPart bdy restOutput =
  let hdrs = (HeaderName "X-Response-Code", maybe "200" show (Rest.responseCode restOutput))
           : mkHeaders (Rest.headersSet restOutput)
  in BodyPart hdrs bdy

-- * Utilities

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def act = runMaybeT act >>= maybe def return
