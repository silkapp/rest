{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Test.FooBar where
import Rest.Client.Internal
import qualified Restexample.Client.Test as Test
import qualified Rest.Types.Error
import qualified Rest.Types.Void
import qualified Rest.StringMap.HashMap.Strict
 
type Identifier = [(Char)]
 
readId :: Identifier -> [String]
readId x = ["id", showUrl x]
 
removeManyId ::
               ApiStateC m =>
               Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (()) ->
                 m (ApiResponse (Rest.Types.Error.Reason (Rest.Types.Void.Void))
                      (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                         (Rest.Types.Error.Status
                            (Rest.Types.Error.Reason (Rest.Types.Void.Void))
                            (()))))
removeManyId input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "DELETE" "v1.0.0" [["test"], ["foo-bar"], ["id"]] []
              rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
remove ::
         ApiStateC m =>
         Identifier -> m (ApiResponse Rest.Types.Void.Void ())
remove fooBar
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0" [["test"], ["foo-bar"], readId fooBar]
              []
              rHeaders
              ""
      in doRequest fromJSON (const ()) request