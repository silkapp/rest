{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import Rest.Gen.Base.ActionInfo
import Rest.Handler
import Rest.Resource
import Rest.Schema

main :: IO ()
main = do
  defaultMain [ testCase "Listing has right parameters." testListingParams
              ]

testListingParams :: Assertion
testListingParams = assertEqual "Parameters" ["offset", "count"] (params actionInfo)
  where
    [actionInfo] = resourceToActionInfo resource
    resource :: Resource IO IO Void () Void
    resource = mkResource { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing id $ \_ -> return []
