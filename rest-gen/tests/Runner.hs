{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Rest.Gen.Base.ActionInfo
import Rest.Handler
import Rest.Resource
import Rest.Schema

main :: IO ()
main = do
  defaultMain [ testCase "Listing has right parameters." testListingParams
              , testCase "Selects should show up only once." testSingleSelect
              ]

testListingParams :: Assertion
testListingParams = assertEqual "Parameters" ["offset", "count"] (params actionInfo)
  where
    [actionInfo] = resourceToActionInfo resource
    resource :: Resource IO IO Void () Void
    resource = mkResource { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing id $ \_ -> return []

testSingleSelect :: Assertion
testSingleSelect = assertEqual "Number of select ActionInfos." 1 (length actionInfos)
  where
    actionInfos = filter isSelect $ resourceToActionInfo resource
    resource :: Resource IO IO ServerId Void Void
    resource = mkResource
      { name   = "resource"
      , schema = noListing $
                   named
                     [ ("id", singleBy ById)
                     , ("ip", singleBy ByIp)
                     ]
      , get    = Just handler_
      , selects = [ ("resources", handler_) ]
      }
    handler_ = mkGetter id $ return ()
    isSelect ai = actionTarget ai == Any && actionType ai == Retrieve && postAction ai

data ServerId = ById String | ByIp String
