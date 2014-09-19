{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Restexample.Client.Post.Comment where
import Rest.Client.Internal
import qualified Restexample.Client.Post as Post
import qualified Rest.Types.Container
import qualified Type.Comment
import qualified Type.UserComment
 
type Identifier = [(Char)]
 
readId :: Identifier -> [String]
readId x = ["id", showUrl x]
 
list ::
       (ApiStateC m) =>
       Post.Identifier ->
         [(String, String)] ->
           m (ApiResponse ()
                (Rest.Types.Container.List (Type.Comment.Comment)))
list post pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["post"], Post.readId post, ["comment"]]
              pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
create ::
         (ApiStateC m) =>
         Post.Identifier ->
           Type.UserComment.UserComment ->
             m (ApiResponse () Type.Comment.Comment)
create post input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["post"], Post.readId post, ["comment"]]
              []
              rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request