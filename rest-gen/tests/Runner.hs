{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Data.String
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H

import Rest.Api
import Rest.Gen.Base.ActionInfo
import Rest.Gen.Base.ApiTree
import Rest.Dictionary (xmlJsonO)
import Rest.Handler
import Rest.Resource
import Rest.Schema

main :: IO ()
main = do
  defaultMain [ testCase "Listing has right parameters." testListingParams
              , testCase "Selects should show up only once." testSingleSelect
              , testCase "Removes should show up only once." testSingleRemove
              , testCase "Listing should have List type." testListingType
              ]

testListingParams :: Assertion
testListingParams = assertEqual "Parameters" ["offset", "count"] (params actionInfo)
  where
    [actionInfo] = resourceToActionInfo resource
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing id $ \_ -> return []

testSingleSelect :: Assertion
testSingleSelect = assertEqual "Number of select ActionInfos." 1 (length actionInfos)
  where
    actionInfos = filter isSelect $ resourceToActionInfo resource
    resource :: Resource IO IO ServerId Void Void
    resource = mkResourceId
      { name   = "resource"
      , schema = noListing $
                   named
                     [ ("id", singleBy ById)
                     , ("ip", singleBy ByIp)
                     ]
      , get    = Just handler_
      , selects = [ ("resources", handler_) ]
      }
    handler_ = mkConstHandler id $ return ()
    isSelect ai = actionTarget ai == Any && actionType ai == Retrieve && postAction ai

testSingleRemove :: Assertion
testSingleRemove = assertEqual "Number of remove ActionInfos." 1 (length actionInfos)
  where
    actionInfos = filter isRemove $ resourceToActionInfo resource
    resource :: Resource IO IO ServerId Void Void
    resource = mkResourceId
      { name   = "resource"
      , schema = noListing $
                   named
                     [ ("id", singleBy ById)
                     , ("ip", singleBy ByIp)
                     ]
      , remove = Just handler_
      }
    handler_ = mkConstHandler id $ return ()
    isRemove ai = actionTarget ai == Self && actionType ai == Delete && postAction ai

data ServerId = ById String | ByIp String

testListingType :: Assertion
testListingType =
  assertEqual "Listing should have List type"
    (Just "Rest.Types.Container.List (())")
    (haskellType . head . outputs . itemInfo . head . resItems $ api)
  where
    api = apiTree (route resource)
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing xmlJsonO $ \_ -> return [()]

instance IsString H.Type where
  fromString = H.fromParseResult . H.parse
