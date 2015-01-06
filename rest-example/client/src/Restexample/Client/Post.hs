{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Post where
import Rest.Client.Internal
import qualified Rest.Types.Container
import qualified Type.Post
import qualified Rest.StringMap.HashMap.Strict
import qualified Rest.Types.Error
import qualified Type.PostError
import qualified Type.UserPost
 
data Identifier = Id Int
                | Latest
 
readId :: Identifier -> [String]
readId (Id x) = ["id", showUrl x]
readId Latest = ["latest"]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse () (Rest.Types.Container.List (Type.Post.Post)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["post"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
byId :: ApiStateC m => Int -> m (ApiResponse () Type.Post.Post)
byId integer
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["post"], ["id"], [showUrl integer]] []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
removeManyId ::
               ApiStateC m =>
               Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (()) ->
                 m (ApiResponse ()
                      (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                         (Rest.Types.Error.Status (Rest.Types.Error.Reason (())) (()))))
removeManyId input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "DELETE" "v1.0.0" [["post"], ["id"]] [] rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
latest :: ApiStateC m => m (ApiResponse () Type.Post.Post)
latest
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["post"], ["latest"]] [] rHeaders ""
      in doRequest fromJSON fromJSON request
 
create ::
         ApiStateC m =>
         Type.UserPost.UserPost ->
           m (ApiResponse Type.PostError.PostError Type.Post.Post)
create input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["post"]] [] rHeaders (toJSON input)
      in doRequest fromJSON fromJSON request
 
remove :: ApiStateC m => Identifier -> m (ApiResponse () ())
remove post
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0" [["post"], readId post] [] rHeaders ""
      in doRequest fromJSON (const ()) request