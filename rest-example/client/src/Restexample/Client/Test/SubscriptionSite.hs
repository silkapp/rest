{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Test.SubscriptionSite where
import Rest.Client.Internal
import qualified Restexample.Client.Test as Test
import qualified Rest.Types.Container
import qualified Rest.StringMap.HashMap.Strict
import qualified Rest.Types.Error

type Identifier = [(Char)]

readId :: Identifier -> [String]
readId x = ["uri", showUrl x]

list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse () (Rest.Types.Container.List (())))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["test"], ["subscription-site"]] pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request

removeManyUri ::
                ApiStateC m =>
                Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (()) ->
                  m (ApiResponse ()
                       (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                          (Rest.Types.Error.Status (Rest.Types.Error.Reason (())) (()))))
removeManyUri input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "DELETE" "v1.0.0"
              [["test"], ["subscription-site"], ["uri"]]
              []
              rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request

remove :: ApiStateC m => Identifier -> m (ApiResponse () ())
remove subscription-site
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0"
              [["test"], ["subscription-site"], readId subscriptionSite]
              []
              rHeaders
              ""
      in doRequest fromJSON (const ()) request
