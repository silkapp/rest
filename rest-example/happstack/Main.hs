{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Trans (liftIO)
import Happstack.Server.SimpleHTTP

import Rest.Driver.Happstack (apiToHandler')

import Api (api)
import ApiTypes (ServerData (..), runBlogApi)
import Example (exampleBlog)

-- | Run the server
main :: IO ()
main = do
  -- Set up the server state
  serverData <- exampleBlog

  -- Start happstack
  putStrLn "Starting happstack server on http://localhost:3000"
  tid <- forkIO $ simpleHTTP (Conf 3000 Nothing Nothing 60 Nothing) (handle serverData)

  -- Exit gracefully
  waitForTermination
  killThread tid

-- | Request handler
handle :: ServerData -> ServerPartT IO Response
handle serverData = apiToHandler' (liftIO . runBlogApi serverData) api

