{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    CPP
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , TypeFamilies
  , UndecidableInstances
  #-}
module Rest.Client.Base
  ( ApiInfo(..)
  , ApiState(..)
  , ApiT(..)
  , Api
  , ApiStateC(..)
  , runT
  , run
  , runWithPort
  , ApiResponse(..)
  , responseToMaybe
  ) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Exception
import Control.Monad.List hiding (mapM)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.RWS hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Writer hiding (mapM)
import Data.ByteString
import Data.CaseInsensitive
import Network.HTTP.Conduit hiding (method, responseBody)

import Rest.Types.Error

data ApiInfo =
  ApiInfo
   { manager :: Manager
   , apiHost :: String
   , apiPort :: Int
   , headers :: [(String, String)]
   }

data ApiState = ApiState { cookies :: CookieJar }

newtype ApiT m a = ApiT { unApiT :: StateT ApiState (ReaderT ApiInfo (ResourceT m)) a }
  deriving ( Functor, Applicative
           , Monad
           , MonadIO
           )

type Api = ApiT IO

class (MonadResource m, MonadBaseControl IO m, Monad m, Functor m, MonadBase IO m, PrimMonad IO) => ApiStateC m where
  getApiState     :: m ApiState
  putApiState     :: ApiState -> m ()
  askApiInfo      :: m ApiInfo

instance (MonadBaseControl IO m, Monad m, Functor m, MonadBase IO m, PrimMonad IO, MonadIO m, MonadThrow m) => ApiStateC (ApiT m) where
  getApiState    = ApiT get
  putApiState    = ApiT . put
  askApiInfo     = ApiT (lift ask)

instance MonadTrans ApiT where
  lift = ApiT . lift . lift . lift

instance MonadBase b m => MonadBase b (ApiT m) where
  liftBase = liftBaseDefault

#if MIN_VERSION_monad_control(1,0,0)
instance MonadTransControl ApiT where
  type StT ApiT a = StT ResourceT (StT (ReaderT ApiInfo) (StT (StateT ApiState) a))
  liftWith f = ApiT (liftWith (\runs -> liftWith (\runrr -> liftWith (\runrs -> f (runrs . runrr . runs . unApiT)))))
  restoreT = ApiT . restoreT . restoreT . restoreT

instance MonadBaseControl v m => MonadBaseControl v (ApiT m) where
  type StM (ApiT m) a = ComposeSt ApiT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
instance MonadTransControl ApiT where
  newtype StT ApiT a = StTApiT { unStTApiT :: StT ResourceT (StT (ReaderT ApiInfo) (StT (StateT ApiState) a)) }
  liftWith f = ApiT (liftWith (\runs -> liftWith (\runrr -> liftWith (\runrs -> f (liftM StTApiT . runrs . runrr . runs . unApiT)))))
  restoreT = ApiT . restoreT . restoreT . restoreT . liftM unStTApiT

instance MonadBaseControl v m => MonadBaseControl v (ApiT m) where
  newtype StM (ApiT m) a = StMApiT { unStMApiT :: ComposeSt ApiT m a }
  liftBaseWith = defaultLiftBaseWith StMApiT
  restoreM     = defaultRestoreM unStMApiT
#endif

instance (MonadException m, MonadBaseControl IO m) => MonadException (ResourceT m) where
  throw     = lift . throw
  catch c f = lift (runResourceT c `catch` (runResourceT . f))

instance (MonadException m, MonadBaseControl IO m) => MonadException (ApiT m) where
  throw     = lift . throw
  catch c f = ApiT (unApiT c `catch` (unApiT . f))

instance MonadThrow m => MonadThrow (ApiT m) where throwM = ApiT . lift . lift . lift . throwM

instance (MonadIO m, MonadThrow m, MonadBase IO m, PrimMonad IO, Functor m, Applicative m) => MonadResource (ApiT m) where
  liftResourceT = ApiT . lift . lift . transResourceT liftIO

instance (Error e, ApiStateC m) => ApiStateC (ErrorT e m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

instance (Monoid w, ApiStateC m) => ApiStateC (RWST r w s m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

instance (Monoid w, ApiStateC m) => ApiStateC (WriterT w m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

instance ApiStateC m => ApiStateC (ListT m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

instance ApiStateC m => ApiStateC (ReaderT r m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

instance ApiStateC m => ApiStateC (StateT s m) where
  getApiState = lift getApiState
  askApiInfo  = lift askApiInfo
  putApiState = lift . putApiState

runT :: (MonadBaseControl IO m, Monad m) => ApiInfo -> ApiState -> ApiT m a -> ResourceT m a
runT inf st api = runReaderT (evalStateT (unApiT api) st) inf

run :: String -> ApiT IO a -> IO a
run = flip runWithPort 80

runWithPort :: String -> Int -> ApiT IO a -> IO a
runWithPort hst prt api =
  withManager $ \m ->
    runT (ApiInfo m hst prt []) (ApiState (createCookieJar [])) api

data ApiResponse e a  =
  ApiResponse
    { statusCode      :: Int
    , statusMessage   :: ByteString
    , httpVersion     :: (Int, Int)
    , responseHeaders :: [(CI ByteString , ByteString)]
    , responseBody    :: Either (Reason e) a
    } deriving Show

responseToMaybe :: ApiResponse e a -> Maybe a
responseToMaybe = either (const Nothing) Just . responseBody
