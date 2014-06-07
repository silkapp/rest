module Main (main) where

import Control.Monad.Trans (liftIO)
import Snap.Http.Server
import Snap.Core

import Rest.Driver.Snap (apiToHandler')

import Api (api)
import ApiTypes (ServerData (..), runBlogApi)
import Example (exampleBlog)

-- | Run the server
main :: IO ()
main = do
  -- Set up the server state
  serverData <- exampleBlog

  -- Setup the Snap configuration.
  let cfg = setPort 3000
          . setAccessLog ConfigNoLog
          . setErrorLog  ConfigNoLog
          $ defaultConfig

  -- Start snap
  httpServe cfg (handle serverData)

-- | Request handler
handle :: ServerData -> Snap ()
handle serverData = apiToHandler' (liftIO . runBlogApi serverData) api

