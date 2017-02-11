{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson
import Data.Monoid
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)
import qualified Data.HashMap.Strict  as H

import Rest hiding (input)
import Rest.Api hiding (route)
import Rest.Dictionary (Format (..), Ident (..))
import Rest.Driver.Perform (accept)
import Rest.Driver.Routing
import Rest.Driver.Types
import Rest.Resource
import qualified Rest.Api          as Rest
import qualified Rest.Container    as C
import qualified Rest.Driver.RestM as RM
import qualified Rest.Resource     as Res
import qualified Rest.Run          as Run

main :: IO ()
main =
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
              , testCase "Multi-POST" testMultiPost
              , testCase "Accept headers." testAcceptHeaders
              , testCase "Test listing count" testListingCount
              ]

listResource :: Resource IO IO Void () Void
listResource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler }
  where
    listHandler () = mkListing id $ \_ -> return []

testListing :: Assertion
testListing = checkRoute GET "resource" (Rest.root -/ Rest.route listResource)

testListingTrailingSlash :: Assertion
testListingTrailingSlash = checkRoute GET "resource/" (Rest.root -/ Rest.route listResource)

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
testUnnamedMulti = checkRoute GET "resource/foo" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO Void String Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Unnamed (Many (Id StringId id))), list = listHandler }
    listHandler (s :: String) = mkListing xmlJsonO $ \_rng -> return (void s)

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
testNamedListing = checkRoute GET "resource/foo" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("foo", Right (Many (Singleton ())))]), list = listHandler }
    listHandler () = mkListing id $ \_rng -> return []

testNamedListingBy :: Assertion
testNamedListingBy = checkRoute GET "resource/foo/bar" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO Void String Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("foo", Right (Many (By (Id StringId id))))]), list = listHandler }
    listHandler (s :: String) = mkListing xmlJsonO $ \_rng -> return (void s)

testCreate :: Assertion
testCreate = checkRoute POST "resource" (root -/ Rest.route resource)
  where
    resource :: Resource IO IO Void Void Void
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named []), create = Just createHandler }
    createHandler = mkConstHandler id $ return ()

-- This one was added because the list handler didn't correctly fall
-- through to create, and testCreate didn't catch that because it
-- contains no list handler.

testCreateWithListing :: Assertion
testCreateWithListing = checkRoutes [(GET, "resource"), (POST, "resource")] (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO Void () Void
    resource = mkResourceId { name = "resource", schema = Schema (Just (Many ())) (Named []), list = listHandler, create = Just createHandler }
    createHandler = mkConstHandler id $ return ()
    listHandler () = mkListing id $ \_rng -> return []

testStaticAction :: Assertion
testStaticAction = checkRoute POST "resource/action" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO IO () Void ()
    resource = mkResourceId { name = "resource", schema = Schema Nothing (Named [("action", Left ())]), statics = staticHandler }
    staticHandler () = mkConstHandler id $ return ()

testSubresource :: Assertion
testSubresource = checkRoute GET "resource/single/subresource" (Rest.root -/ Rest.route resource --/ Rest.route subResource)
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
testMultiPut = checkRouteSuccess PUT "resource/foo" (Rest.root -/ Rest.route resource)
  where
    resource :: Resource IO (ReaderT String IO) String Void Void
    resource = mkResourceReader
      { name   = "resource"
      , schema = Schema Nothing (Named [("foo", Right (Single (By (Id StringId id))))])
      , update = Just (mkConstHandler xmlJsonO (fmap void ask))
      }

testMultiPost :: Assertion
testMultiPost = checkRoute POST "" (Rest.root :: Rest.Router IO IO)

type Uri = String

checkSingleRoute :: (Applicative m, Monad m, Monad s)
                 => Uri -> Resource m s sid mid aid -> Handler s -> Assertion
checkSingleRoute uri resource handler_ =
  do checkRoute GET    uri (root -/ Rest.route resource { get = Just handler_ })
     checkRoute PUT    uri (root -/ Rest.route resource { update = Just handler_ })
     checkRoute DELETE uri (root -/ Rest.route resource { remove = Just handler_ })
     checkRoute GET    (uri <> "/select") (root -/ Rest.route resource { selects = [("select", handler_)] })
     checkRoute POST   (uri <> "/action") (root -/ Rest.route resource { actions = [("action", handler_)] })

checkRoute :: (Applicative m, Monad m) => Method -> Uri -> Rest.Router m s -> Assertion
checkRoute method uri router = checkRouteWithIgnoredMethods [method] router method uri

checkRoutes :: (Applicative m, Monad m) => [(Method, Uri)] -> Rest.Router m s -> Assertion
checkRoutes reqs router =
  forM_ reqs $ uncurry $ checkRouteWithIgnoredMethods (map fst reqs) router

checkRouteWithIgnoredMethods :: (Applicative m, Monad m) => [Method] -> Rest.Router m s -> Method -> Uri -> Assertion
checkRouteWithIgnoredMethods ignoredMethods router method uri =
  do checkRouteSuccess method uri router
     forM_ (filter (not . (`elem` ignoredMethods)) allMethods) $ \badMethod -> checkRouteFailure badMethod uri router
     checkRouteFailure method (uri <> "/trailing") router

checkRouteFailure :: (Applicative m, Monad m) => Method -> Uri -> Rest.Router m s -> Assertion
checkRouteFailure method uri router =
  case route (Just method) (splitUriString $ "v1.0/" <> uri) (Versioned [(Version 1 0 Nothing, Some1 router)]) of
    Left _  -> return ()
    Right _ -> assertFailure ("Should be no route to " ++ show method ++ " " ++ uri ++ ".")

checkRouteSuccess :: (Applicative m, Monad m) => Method -> Uri -> Rest.Router m s -> Assertion
checkRouteSuccess method uri router =
  case route (Just method) (splitUriString $ "v1.0/" <> uri) (Versioned [(Version 1 0 Nothing, Some1 router)]) of
    Left e  -> assertFailure ("No route to " ++ show method ++ " " ++ uri ++ ": " ++ show e)
    Right _ -> return ()

allMethods :: [Method]
allMethods = [GET, PUT, POST, DELETE]

testAcceptHeaders :: Assertion
testAcceptHeaders =
  do let fmt = accept (Just "text/json") Nothing Nothing
     assertEqual "Accept json format." [JsonFormat] fmt

testListingCount :: Assertion
testListingCount = assertEqual "listing count" (Right $ C.List 0 5 [1..5::Int]) (eitherDecode bs)
  where
    (bs, _meta) = runIdentity . RM.runRestM input $ Run.apiToHandler api
    input :: RM.RestInput
    input = RM.emptyInput
      { RM.parameters = H.fromList [("count", "5")]
      , RM.paths      = splitUriString "resource"
      , RM.headers    = H.fromList
          [ ("Accept"      , "application/json")
          , ("Content-Type", "application/json")
          ]
      }
    api :: Api (RM.RestM Identity)
    api = Unversioned (Some1 $ Rest.root -/ Rest.route resource)
    resource :: Resource (RM.RestM Identity) (RM.RestM Identity) String () Void
    resource = mkResourceId
      { Res.name   = "resource"
      , Res.schema = withListing () $ named []
      , Res.list   = const listHandler
      }
      where
      listHandler :: ListHandler (RM.RestM Identity)
      listHandler = mkListing jsonO h
        where
          h :: Range -> ExceptT Reason_ (RM.RestM Identity) [Int]
          h _rng = return [1..10]
