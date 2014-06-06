{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.STM (newTVarIO)
import Data.Set (Set)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Set as Set

import Rest.Driver.Wai (apiToApplication)

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

  -- Start warp
  putStrLn "Starting warp server on http://localhost:3000"
  run 3000 (handle serverData)

-- | Request handler
handle :: ServerData -> Application
handle serverData = apiToApplication (runBlogApi serverData) api

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

