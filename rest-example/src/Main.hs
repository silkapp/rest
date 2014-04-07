{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import Happstack.Server.SimpleHTTP
import qualified Data.Set as Set

import Rest.Driver.Happstack (apiToHandler')

import Api (api)
import ApiTypes (ServerData (..), runBlogApi)
import Type.Post (Post (Post))
import Type.User (User (User))

-- | Run the server
main :: IO ()
main = do
  -- Set up the server state
  usrs <- newTVarIO mockUsers
  psts <- newTVarIO mockPosts
  let serverData = ServerData
       { users = usrs
       , posts = psts
       }

  -- Start happstack
  tid <- forkIO $ simpleHTTP (Conf 3000 Nothing Nothing 60 Nothing) (handle serverData)

  -- Exit gracefully
  waitForTermination
  killThread tid

-- | Request handler
handle :: ServerData -> ServerPartT IO Response
handle serverData = toResponse <$> apiToHandler' (lift . runBlogApi serverData) api

-- | Prepoulated users
mockUsers :: Set User
mockUsers = Set.fromList
  [ User "adam" "1234"
  , User "erik" "2345"
  , User "sebas" "3456"
  ]

-- | Prepopulated posts
mockPosts :: Set Post
mockPosts = Set.fromList
  [ Post "adam" (read "2014-03-31 15:34:00") "First post" "Hello world!"
  ]
