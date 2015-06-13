module Rest.Gen.JSCompiler (Compiler (..), withCompiler) where

import qualified Network.HTTP                 as HTTP
import qualified Network.HTTP.Base            as HTTP
import qualified Network.URI                  as HTTP
import Data.Functor
import Data.Maybe


import Code.Build

data Compiler = ClosureWhitespace | ClosureSimple | ClosureAdvanced 

withCompiler:: Maybe Compiler -> String -> IO String
withCompiler Nothing = return . id
withCompiler (Just compiler)  = useCompiler compiler

useCompiler:: Compiler -> String -> IO String
useCompiler ClosureSimple = closureCompile "SIMPLE_OPTIMIZATIONS"
useCompiler ClosureAdvanced = closureCompile "ADVANCED_OPTIMIZATIONS"
useCompiler ClosureWhitespace = closureCompile "WHITESPACE_ONLY"


closureCompile:: String -> String -> IO String
closureCompile compilationlevel code_str = HTTP.simpleHTTP (mkPostRequest) >>=  HTTP.getResponseBody
  where payload = HTTP.urlEncodeVars [("js_code",code_str), ("compilation_level",compilationlevel),("output_format","text"),("output_info","compiled_code")]
        headers = [HTTP.mkHeader HTTP.HdrContentType "application/x-www-form-urlencoded", HTTP.mkHeader HTTP.HdrContentLength (show $ length payload)]
        url = HTTP.parseURI "http://www.closure-compiler.appspot.com/compile"
        mkPostRequest = HTTP.Request {HTTP.rqURI = fromJust url, HTTP.rqMethod = HTTP.POST, HTTP.rqHeaders = headers, HTTP.rqBody = payload }

