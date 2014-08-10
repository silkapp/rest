-- | The API path hierarchy
module Api where

import Rest.Api

import ApiTypes (BlogApi)
import qualified Rest.Discovery.Api.ApiDescription as ApiDescription
import qualified Api.Post as Post
import qualified Api.User as User
import qualified Api.Post.Comment as Post.Comment

-- | Defines a versioned api
api :: Api BlogApi
api = [(mkVersion 1 0 0, Some1 blog)]

-- _ The entire routing table for v1.0.0 of the blog
blog :: Router BlogApi BlogApi
blog =
  root -/ user
       -/ post --/ comment
        -/ route (ApiDescription.resource blog)
  where
    user = route User.resource
    post = route Post.resource
    comment = route Post.Comment.resource
