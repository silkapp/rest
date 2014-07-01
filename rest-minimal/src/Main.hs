module Main where

import Control.Monad.Reader
import Data.IORef
import Network.Wai.Handler.Warp
import Rest
import Rest.Api
import Rest.Driver.Wai
import Rest.Gen
import Rest.Gen.Config
import System.Environment
import qualified Rest.Resource as R

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Starting warp server on http://localhost:3000"
      st <- initialState
      run 3000 $
        apiToApplication (runApi st) api
    "--gen":args' ->
      withArgs args' $ do
        config <- configFromArgs "minimal"
        generate config "Minimal" api [] [] []
    _ -> error "call with --gen or without arguments"

api :: Api ApiState
api = [(mkVersion 1 0 0, Some1 router)]

router :: Router ApiState ApiState
router =
  root -/ route messageResource

type ApiState = ReaderT (IORef [Int]) IO

runApi :: IORef [Int] -> ApiState a -> IO a
runApi s b = runReaderT b s

initialState :: IO (IORef [Int])
initialState = newIORef [0]

messageResource :: Resource ApiState ApiState Int () Void
messageResource = mkResourceId
  { R.name   = "message"
  , R.schema = withListing () $ named []
  , R.list   = const getList
  , R.create = Just setMessage
  }

getList :: ListHandler ApiState
getList = mkListing (jsonO . someO) $ const (liftIO . readIORef =<< ask)

setMessage :: Handler ApiState
setMessage = mkInputHandler (jsonI . someI) $ \n -> do
  ref <- ask
  liftIO $ modifyIORef ref (n :)
