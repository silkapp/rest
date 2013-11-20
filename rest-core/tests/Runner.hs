{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure)

import qualified Data.ByteString.Char8 as Char8

import Rest.Api hiding (route)
import Rest.Driver.Routing
import Rest.Handler
import Rest.Resource
import Rest.Schema
import Rest.Dictionary
import qualified Rest.Api as Rest

main :: IO ()
main = do
  defaultMain [ testCase "Top level listing." testListing
              , testCase "Top level listing (trailing slash)." testListingTrailingSlash
              , testCase "Top level singleton." testToplevelSingleton
              , testCase "Unnamed single." testUnnamedSingle
              , testCase "Unnamed multi." testUnnamedMulti
              , testCase "Named singleton." testNamedSingleton
              , testCase "Named single by." testNamedSingleBy
              , testCase "Named listing." testNamedListing
              , testCase "Named listing by." testNamedListingBy
              , testCase "Create." testCreate
              , testCase "Create with listing." testCreateWithListing
              , testCase "Static action." testStaticAction
              , testCase "Simple subresource." testSubresource
              , testCase "Root router is skipped." testRootRouter
              , testCase "Multi-PUT." testMultiPut
              ]

testListing :: Assertion
testListing = checkRoute GET "resource" (Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing id $ \_ -> return []

testListingTrailingSlash :: Assertion
testListingTrailingSlash = checkRoute GET "resource/" (Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    listHandler () = mkListing id $ \_ -> return []

testToplevelSingleton :: Assertion
testToplevelSingleton = checkSingleRoute "resource" resource handler_
  where
    resource :: Resource IO IO () Void Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Single ())) (Named []) }
    handler_ = mkConstHandler id $ return ()

testUnnamedSingle :: Assertion
testUnnamedSingle = checkSingleRoute "resource/foo" resource handler_
  where
    resource :: Resource IO (ReaderT String IO) String Void Void
    resource = mkResourceReader { name = "resource", schema = Schema Nothing (Unnamed (Single (Id StringId id))) }
    handler_ = mkConstHandler stringO ask

testUnnamedMulti :: Assertion
testUnnamedMulti = checkRoute GET "resource/foo" (Rest.route resource)
  where
    resource :: Resource IO IO Void String Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Unnamed (Many (Id StringId id))), list = listHandler }
    listHandler (s :: String) = mkListing stringO $ \_rng -> return [s]

testNamedSingleton :: Assertion
testNamedSingleton = checkSingleRoute "resource/foo" resource handler_
  where
    resource :: Resource IO IO () Void Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("foo", Right (Single (Singleton ())))]) }
    handler_ = mkConstHandler id $ return ()

testNamedSingleBy :: Assertion
testNamedSingleBy = checkSingleRoute "resource/foo/bar" resource handler_
  where
    resource :: Resource IO (ReaderT String IO) String Void Void
    resource = mkResourceReader { name = "resource", schema = Schema Nothing (Named [("foo", Right (Single (By (Id StringId id))))]) }
    handler_ = mkConstHandler stringO ask

testNamedListing :: Assertion
testNamedListing = checkRoute GET "resource/foo" (Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("foo", Right (Many (Singleton ())))]), list = listHandler }
    listHandler () = mkListing id $ \_rng -> return []

testNamedListingBy :: Assertion
testNamedListingBy = checkRoute GET "resource/foo/bar" (Rest.route resource)
  where
    resource :: Resource IO IO Void String Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("foo", Right (Many (By (Id StringId id))))]), list = listHandler }
    listHandler (s :: String) = mkListing stringO $ \_rng -> return [s]

testCreate :: Assertion
testCreate = checkRoute POST "resource" (Rest.route resource)
  where
    resource :: Resource IO IO Void Void Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named []), create = Just createHandler }
    createHandler = mkConstHandler id $ return ()

-- This one was added because the list handler didn't correctly fall
-- through to create, and testCreate didn't catch that because it
-- contains no list handler.

testCreateWithListing :: Assertion
testCreateWithListing = checkRoutes [(GET, "resource"), (POST, "resource")] (Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler, create = Just createHandler }
    createHandler = mkConstHandler id $ return ()
    listHandler () = mkListing id $ \_rng -> return []

testStaticAction :: Assertion
testStaticAction = checkRoute POST "resource/action" (Rest.route resource)
  where
    resource :: Resource IO IO () Void ()
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("action", Left ())]), statics = staticHandler }
    staticHandler () = mkConstHandler id $ return ()

testSubresource :: Assertion
testSubresource = checkRoute GET "resource/single/subresource" (Rest.route resource -/ Rest.route subResource)
  where
    resource :: Resource IO IO () Void Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("single", Right (Single (Singleton ())))]), get = Just getHandler }
    subResource :: Resource IO IO Void () Void
    subResource = mkResourceId { name = "subresource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
    getHandler = mkConstHandler id $ return ()
    listHandler _ = mkListing id $ \_rng -> return []

testRootRouter :: Assertion
testRootRouter = checkRoute GET "resource/single" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO () Void Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("single", Right (Single (Singleton ())))]), get = Just getHandler }
    getHandler = mkConstHandler id $ return ()

testMultiPut :: Assertion
testMultiPut = checkRouteSuccess PUT "resource/foo" (Rest.route resource)
  where
    resource :: Resource IO (ReaderT String IO) String Void Void
    resource = mkResourceReader
      { name   = "resource"
      , schema = Schema Nothing (Named [("foo", Right (Single (By (Id StringId id))))])
      , update = Just (mkConstHandler stringO ask)
      }

checkSingleRoute :: Monad s => Uri -> Resource m s sid mid aid -> Handler s -> Assertion
checkSingleRoute uri resource handler_ =
  do checkRoute GET    uri (Rest.route resource { get = Just handler_ })
     checkRoute PUT    uri (Rest.route resource { update = Just handler_ })
     checkRoute DELETE uri (Rest.route resource { remove = Just handler_ })
     checkRoute GET    (uri <> "/select") (Rest.route resource { selects = [("select", handler_)] })
     checkRoute POST   (uri <> "/action") (Rest.route resource { actions = [("action", handler_)] })

checkRoute :: Method -> Uri -> Rest.Router m s -> Assertion
checkRoute method uri router = checkRouteWithIgnoredMethods [method] router method uri

checkRoutes :: [(Method, Uri)] -> Rest.Router m s -> Assertion
checkRoutes reqs router =
  do forM_ reqs $ uncurry $ checkRouteWithIgnoredMethods (map fst reqs) router

checkRouteWithIgnoredMethods :: [Method] -> Rest.Router m s -> Method -> Uri -> Assertion
checkRouteWithIgnoredMethods ignoredMethods router method uri =
  do checkRouteSuccess method uri router
     forM_ (filter (not . (`elem` ignoredMethods)) allMethods) $ \badMethod -> checkRouteFailure badMethod uri router
     checkRouteFailure method (uri <> "/trailing") router

checkRouteFailure :: Method -> Uri -> Rest.Router m s -> Assertion
checkRouteFailure method uri router =
  case route method (splitUri $ "v1.0/" <> uri) [(Version 1 0 Nothing, Some1 router)] of
    Left _  -> return ()
    Right _ -> assertFailure ("Should be no route to " ++ show method ++ " " ++ Char8.unpack uri ++ ".")

checkRouteSuccess :: Method -> Uri -> Rest.Router m s -> Assertion
checkRouteSuccess method uri router =
  case route method (splitUri $ "v1.0/" <> uri) [(Version 1 0 Nothing, Some1 router)] of
    Left e  -> assertFailure ("No route to " ++ show method ++ " " ++ Char8.unpack uri ++ ": " ++ show e)
    Right _ -> return ()

allMethods :: [Method]
allMethods = [GET, PUT, POST, DELETE]
