{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.User where
import Rest.Client.Internal
import qualified Rest.Types.Container
import qualified Type.UserInfo
import qualified Type.UserSignupError
import qualified Type.User
import qualified Data.Text
 
type Identifier = Data.Text.Text
 
readId :: Identifier -> [String]
readId x = ["name", showUrl x]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse ()
              (Rest.Types.Container.List (Type.UserInfo.UserInfo)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["user"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
create ::
         ApiStateC m =>
         Type.User.User ->
           m (ApiResponse Type.UserSignupError.UserSignupError
                Type.UserInfo.UserInfo)
create input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["user"]] [] rHeaders (toJSON input)
      in doRequest fromJSON fromJSON request