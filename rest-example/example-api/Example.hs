{-# LANGUAGE OverloadedStrings #-}
module Example (exampleBlog) where

import Control.Applicative
import Control.Concurrent.STM (newTVarIO)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as Set

import ApiTypes (ServerData (..))
import Type.Comment (Comment (Comment))
import Type.Post (Post (Post))
import Type.User (User (User))

-- Set up the server state
exampleBlog :: IO ServerData
exampleBlog = ServerData
          <$> newTVarIO mockUsers
          <*> newTVarIO mockPosts
          <*> newTVarIO mockComments

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
  [ Post 0 "adam" (read "2014-03-31 15:34:00") "First post" "Hello world!"
  , Post 1 "erik" (read "2014-04-01 13:37:00") "Rest is awesome" "Just wanted to tell the world!"
  ]

mockComments :: HashMap Int (Set Comment)
mockComments = H.fromList
  [(0, Set.fromList [Comment "adam" (read "2014-06-08 14:00:00") "This is the best post I've ever written, please be gentle"])]
