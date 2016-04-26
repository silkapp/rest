-- | The API path hierarchy
module Api where

import Rest.Api

import ApiTypes (BlogApi)
import qualified Api.Post              as Post
import qualified Api.Post.Comment      as Post.Comment
import qualified Api.Test              as Test
import qualified Api.Test.DashedName   as DashedName
import qualified Api.Test.ReservedName as ReservedName
import qualified Api.User              as User

-- | Defines a versioned api
api :: Api BlogApi
api = Versioned [(mkVersion 1 0 0, Some1 blog)]

-- | The entire routing table for v1.0.0 of the blog
blog :: Router BlogApi BlogApi
blog =
  root -/ user
       -/ post --/ comment
       -/ test --/ reservedName
               --/ dashedName
  where
    comment      = route Post.Comment.resource
    dashedName   = route DashedName.resource
    post         = route Post.resource
    reservedName = route ReservedName.resource
    test         = route Test.resource
    user         = route User.resource
