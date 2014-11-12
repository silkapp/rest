{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Test where
import Rest.Client.Internal
import qualified Api.Test
import qualified Api.Test.Err2
 
noResponse :: ApiStateC m => m (ApiResponse () ())
noResponse
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["noResponse"]] [] rHeaders ""
      in doRequest fromJSON (const ()) request
 
onlyError :: ApiStateC m => m (ApiResponse Api.Test.Err ())
onlyError
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["onlyError"]] [] rHeaders ""
      in doRequest fromJSON (const ()) request
 
differentFormats ::
                   ApiStateC m => String -> m (ApiResponse Api.Test.Err Api.Test.Ok)
differentFormats input
  = let rHeaders
          = [(hAccept, "text/json,text/xml"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["differentFormats"]] []
              rHeaders
              (fromString input)
      in doRequest fromJSON fromXML request
 
intersectedFormats ::
                     ApiStateC m => String -> m (ApiResponse Api.Test.Err Api.Test.Ok)
intersectedFormats input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["intersectedFormats"]] []
              rHeaders
              (fromString input)
      in doRequest fromJSON fromJSON request
 
intersectedFormats2 ::
                      ApiStateC m => String -> m (ApiResponse Api.Test.Err Api.Test.Ok)
intersectedFormats2 input
  = let rHeaders
          = [(hAccept, "text/xml"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["intersectedFormats2"]] []
              rHeaders
              (fromString input)
      in doRequest fromXML fromXML request
 
errorImport ::
              ApiStateC m => String -> m (ApiResponse Api.Test.Err2.Err String)
errorImport input
  = let rHeaders
          = [(hAccept, "text/xml"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["errorImport"]] [] rHeaders
              (fromString input)
      in doRequest fromXML fromXML request
 
noError :: ApiStateC m => m (ApiResponse () Api.Test.Ok)
noError
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["noError"]] [] rHeaders ""
      in doRequest fromJSON fromJSON request
 
justStringO :: ApiStateC m => m (ApiResponse () String)
justStringO
  = let rHeaders
          = [(hAccept, "text/plain,text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["justStringO"]] [] rHeaders
              ""
      in doRequest fromJSON toString request
 
preferJson ::
             ApiStateC m => String -> m (ApiResponse Api.Test.Err Api.Test.Ok)
preferJson input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["preferJson"]] [] rHeaders
              (fromString input)
      in doRequest fromJSON fromJSON request
 
octetStreamOut ::
                 ApiStateC m =>
                 ByteString -> m (ApiResponse Api.Test.Err ByteString)
octetStreamOut input
  = let rHeaders
          = [(hAccept, "text/json,application/octet-stream"),
             (hContentType, "application/octet-stream")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["octetStreamOut"]] []
              rHeaders
              (id input)
      in doRequest fromJSON id request
 
onlyInput :: ApiStateC m => () -> m (ApiResponse () ())
onlyInput input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["test"], ["onlyInput"]] [] rHeaders
              (toJSON input)
      in doRequest fromJSON (const ()) request