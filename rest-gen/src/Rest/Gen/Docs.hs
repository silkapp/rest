{-# LANGUAGE TupleSections #-}
module Rest.Gen.Docs
  ( DocsContext (..)
  , cdiv
  , cls
  , mkAllResources
  , mkSingleResource
  , resourcesInfo
  , row
  , subResourcesInfo
  , writeDocs
  ) where

import Prelude.Compat hiding (div, head, span, (.))
import qualified Prelude.Compat as P

import Control.Category ((.))
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Hashable (hash)
import Data.List.Compat hiding (head, span)
import Data.String.Compat
import System.Directory
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, meta, style)
import Text.Blaze.Html5.Attributes hiding (id, method, span, title)
import Text.Blaze.Html.Renderer.String
import Text.StringTemplate
import qualified Data.Label.Total            as L
import qualified Text.Blaze.Html5.Attributes as A

import Rest.Api (Router, Version)
import Rest.Gen.Base
import Rest.Gen.Utils

-- | Information about the context in which a resource is contained
data DocsContext = DocsContext
  { rootUrl        :: String
  , contextVersion :: Version
  , templates      :: String
  } deriving (Eq, Show)

writeDocs :: DocsContext -> Router m s -> String -> IO ()
writeDocs context router loc =
  do createDirectoryIfMissing True loc
     let tree = apiSubtrees router
     mkAllResources context tree >>= writeFile (loc </> "index.html")
     mapM_ (writeSingleResource context loc) $ allSubResources tree

writeSingleResource :: DocsContext -> String -> ApiResource -> IO ()
writeSingleResource ctx loc r =
  do let dr = loc </> intercalate "/" (resId r)
     createDirectoryIfMissing True dr
     mkSingleResource ctx r >>= writeFile (dr </> "index.html")

mkAllResources :: DocsContext -> ApiResource -> IO String
mkAllResources ctx tree =
  do tmpls <- directoryGroup (templates ctx)
     tmpl  <- maybe (putStrLn "Couldn't find template api-docs-all" >> return (newSTMP "")) return $
                     getStringTemplate "api-docs-all" tmpls
     return $ render
            $ setManyAttrib
                   [ ("listing"  , map (renderHtml . (\v -> resourceLinkAnchor v (resourceDisp v))) . sort . allSubResourceIds $ tree) ]
            $ setManyAttrib
                   [ ("resources" , renderHtml $ subResourcesInfo ctx tree )
                   , ("version"   , show $ contextVersion ctx  )
                   , ("static"    , rootUrl ctx  )
                   ]
                   tmpl

mkSingleResource :: DocsContext -> ApiResource -> IO String
mkSingleResource ctx tree =
  do tmpls <- directoryGroup (templates ctx)
     tmpl  <- maybe (putStrLn "Couldn't find template api-docs-resource" >> return (newSTMP "")) return $
                     getStringTemplate "api-docs-resource" tmpls
     return $ render
            $ setManyAttrib
                   [ ("subresources", map (renderHtml . (\v -> resourceLinkRemote (rootUrl ctx) v (resourceDisp v))) $ allResourceIds tree)
                   , ("resource"    , resId tree)
                   , ("parents"     , map (renderHtml . (\v -> resourceLinkRemote (rootUrl ctx) v (toHtml $ last v))) $ tail $ inits (resId tree))
                   , ("identifiers" , map renderHtml $ resourceIdentifiers (resLink tree) (resIdents tree))
                   ]
            $ setManyAttrib
                   [ ("name"        , resName tree)
                   , ("urls"        , renderHtml $ resourceTable tree)
                   , ("description" , resDescription tree)
                   , ("version"     , show $ contextVersion ctx)
                   , ("static"      , rootUrl ctx  )
                   ]
                   tmpl

-- | Helper functions for generating HTML
cls :: String -> Attribute
cls = class_ . toValue

cdiv :: String -> Html -> Html
cdiv s = div ! cls s

row :: Html -> Html
row = cdiv "row"

-- | Recursively generate information for a resource structure
resourcesInfo :: DocsContext -> ApiResource -> Html
resourcesInfo ctx = foldTree $ \it -> sequence_ . (resourceInfo ctx it :)

subResourcesInfo :: DocsContext -> ApiResource -> Html
subResourcesInfo ctx = foldTreeChildren sequence_ $ \it -> sequence_ . (resourceInfo ctx it :)

-- | Generate information for one resource
resourceInfo :: DocsContext -> ApiResource -> Html
resourceInfo ctx it = section $
  do resourceAnchor (resId it)
     row $ cdiv "span16 page-header resource-title" $ h1 $ resourceLinkRemote (rootUrl ctx) (resId it) $ resourceDisp (resId it)
     row $
      do cdiv "span10" $
           do h2 $ toHtml "Description"
              p $ toHtml $ resDescription it
         cdiv "span6" $
           do h2 $ toHtml "Identifiers"
              p $ sequence_ $ intersperse br $ resourceIdentifiers (resLink it) (resIdents it)
     br
     resourceTable it

resourceIdentifiers :: Link -> [Link] -> [Html]
resourceIdentifiers lnk lnks =
  case lnks of
    [] -> [toHtml "No identifiers"]
    ls -> map (linkHtml . (lnk ++)) ls

resourceTable :: ApiResource -> Html
resourceTable it =
  let urlInfo = groupByFirst . concatMap (\ai -> map (,itemInfo ai) $ flattenLast $ itemLink ai) $ resItems it
  in table ! cls "bordered-table resource-table" $
      do thead $ mapM_ (\v -> th ! cls v $ toHtml v) ["URL", "Method", "Description", "Input", "Output", "Errors", "Parameters"]
         tbody $ forM_ (zip [(1 :: Int)..] urlInfo) $ \(n, (url, ais)) ->
          do tr ! cls ("stripe-" ++ show (n `mod` 2) ++ " url-main-row") $ mapM_ td
                     [ linkHtml url
                     , toHtml $ show $ method $ P.head ais
                     , toHtml $ mkActionDescription (resName it) $ P.head ais
                     , dataDescriptions "None"  $ inputs $ P.head ais
                     , dataDescriptions "None" $ outputs $ P.head ais
                     , dataDescriptions "None" $ errors $ P.head ais
                     , toHtml $ if null (params (P.head ais)) then "None" else intercalate ", " $ params $ P.head ais
                     ]
             forM_ (tail ais) $ \ai ->
                tr ! cls ("stripe-" ++ show (n `mod` 2) ++ " url-data-row") $ mapM_ td
                     [ return ()
                     , toHtml $ show $ method ai
                     , toHtml $ mkActionDescription (resName it) ai
                     , dataDescriptions "None"  (inputs ai)
                     , dataDescriptions "None" (outputs ai)
                     , dataDescriptions "None" (errors ai)
                     , toHtml $ if null (params ai) then "None" else intercalate ", " $ params ai
                     ]

-- | Generate information for input/output data structure
dataDescriptions :: String -> [DataDescription] -> Html
dataDescriptions s []    = toHtml s
dataDescriptions _ descs =
  table ! cls "data-description" $
    do tr $ forM_ descs $ \dsc -> td $ toHtml . L.get (dataTypeDesc . meta) $ dsc
       tr $ forM_ descs $ \dsc -> td $
        do forM_ (L.get (dataSchema  . meta) dsc) $ mkCode (typeLang (L.get (dataType . desc) dsc)) "Schema"
           forM_ (L.get (dataExample . meta) dsc) $ mkCode (typeLang (L.get (dataType . desc) dsc)) "Example"
  where typeLang XML  = "xml"
        typeLang JSON = "js"
        typeLang _    = ""

-- | Helper function for setting the right attributes to make something collapsible.
-- The javascript prt can be found in docs.js

mkCode :: String -> String -> String -> Html
mkCode lng cap cd =
  let eid = "idv" ++ show (hash cd)
  in do div ! cls "modal hide fade code" ! A.id (toValue eid) $
          do cdiv "modal-header" $
              do a ! href (toValue "#") ! cls "close" $ toHtml "x"
                 h3 $ toHtml cap
             cdiv "modal-body" $
               div ! style (toValue "overflow:auto; max-height:600px") $
                 pre ! cls ("prettyprint lang-" ++ lng) $ toHtml cd
        button ! cls "btn open-modal"
               ! customAttribute (fromString "data-controls-modal") (toValue eid)
               ! customAttribute (fromString "data-backdrop") (toValue "true")
               ! customAttribute (fromString "data-keyboard") (toValue "true")
               $ toHtml cap

resourceId :: ResourceId -> String
resourceId = intercalate "."

resourceDisp :: ResourceId -> Html
resourceDisp = toHtml . intercalate "/"

resourceLinkAnchor :: ResourceId -> Html -> Html
resourceLinkAnchor rid = a ! cls "resource-link" ! href (toValue $ "#" ++ resourceId rid)

resourceLinkRemote :: String -> ResourceId -> Html -> Html
resourceLinkRemote rUrl rid = a ! cls "resource-link" ! href (toValue $ rUrl ++ intercalate "/" rid)

resourceAnchor :: ResourceId -> Html
resourceAnchor rid = a ! name (toValue $ resourceId rid) $ return ()

linkHtml :: Link -> Html
linkHtml =  mapM_ linkItem
  where linkItem (LParam idf)   = toHtml ("/<" ++ idf ++ ">")
        linkItem (LAccess lnks) = span ! class_ (toValue "link-block") $ sequence_ $ intersperse br
                                   $ map linkHtml $ sortBy (flip compare `on` length) lnks
        linkItem x              = toHtml ("/" ++ itemString x)
