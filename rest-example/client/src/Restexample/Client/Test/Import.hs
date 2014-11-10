{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Test.Import where
import Rest.Client.Internal
import qualified Restexample.Client.Test as Test
 
type Identifier = String
 
readId :: Identifier -> [String]
readId x = ["it", showUrl x]
 
byIt :: ApiStateC m => String -> m (ApiResponse () ())
byIt string
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0"
              [["test"], ["import"], ["it"], [showUrl string]]
              []
              rHeaders
              ""
      in doRequest fromJSON (const ()) request
 
do_ :: ApiStateC m => Identifier -> m (ApiResponse () ())
do_ import_
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0"
              [["test"], ["import"], readId import_, ["do"]]
              []
              rHeaders
              ""
      in doRequest fromJSON (const ()) request