{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
   DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , UndecidableInstances
  #-}
module Rest.Types.Container
  ( List(..)
  , Map(..)
  ) where

import Control.Arrow
import Data.JSON.Schema hiding (key)
import Data.JSON.Schema.Combinators (field)
import Data.Text (Text)
import Data.Typeable
import Generics.Regular (deriveAll, PF)
import Generics.Regular.JSON
import Generics.Regular.XmlPickler (gxpickle)
import Text.JSON
import Text.XML.HXT.Arrow.Pickle
import Data.String
import Data.String.ToString

-------------------------------------------------------------------------------

data List a = List
  { offset :: Int
  , count  :: Int
  , items  :: a
  } deriving (Show, Typeable)

deriveAll ''List "PFList"
type instance PF (List a) = PFList a

instance XmlPickler a => XmlPickler (List a) where
  xpickle = gxpickle `const` (offset, count, items)

instance JSON a => JSON (List a) where
 showJSON = gshowJSON
 readJSON = greadJSON

instance JSONSchema a => JSONSchema (List a)  where
  schema = gSchema

instance Json a => Json (List a)

-------------------------------------------------------------------------------

deriveAll ''Text "PFText"
type instance PF Text = PFText

instance XmlPickler Text where
  xpickle = xpWrap (undefined, undefined) xpText0

-------------------------------------------------------------------------------

newtype Map a b = Map { unMap :: [(a, b)] } deriving (Show, Typeable)

deriveAll ''Map "PFMap"
type instance PF (Map a b) = PFMap a b

instance (IsString a, ToString a, XmlPickler b) => XmlPickler (Map a b) where
  xpickle = xpElem "map" (xpWrap (Map, unMap) (xpList (xpPair (xpElem "key" (xpWrap (fromString,toString) xpText)) xpickle)))

instance (ToString a, IsString a, JSON b) => JSON (Map a b) where
  showJSON = showJSON . toJSObject . map (first toString) . unMap
  readJSON = fmap (Map . map (first fromString) . fromJSObject) . readJSON

instance (IsString a, ToString a, JSONSchema b) => JSONSchema (Map a b) where
  schema _ = field "key" False (schema (Proxy :: Proxy b))

instance (IsString a, ToString a, Json b) => Json (Map a b)

