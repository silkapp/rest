{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Test.FooBar where
import Rest.Client.Internal
import qualified Restexample.Client.Test as Test
import qualified Rest.StringMap.HashMap.Strict
import qualified Rest.Types.Error
 
type Identifier = [(Char)]
 
readId :: Identifier -> [String]
readId x = ["id", showUrl x]
 
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
          = makeReq "DELETE" "v1.0.0" [["test"], ["foo-bar"], ["id"]] []
              rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
remove :: ApiStateC m => Identifier -> m (ApiResponse () ())
remove fooBar
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0" [["test"], ["foo-bar"], readId fooBar]
              []
              rHeaders
              ""
      in doRequest fromJSON (const ()) request