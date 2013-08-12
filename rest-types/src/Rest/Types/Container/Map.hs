{-# LANGUAGE
    TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  , FlexibleInstances
  , ScopedTypeVariables
  , DeriveDataTypeable
  , GADTs
  #-}
module Rest.Types.Container.Map where

import Control.Arrow
import Data.JSON.Schema hiding (key)
import Data.JSON.Schema.Combinators (field)
import Data.Typeable
import Generics.Regular (deriveAll, PF)
import Generics.Regular.JSON
import Generics.Regular.XmlPickler (gxpickle)
import Text.JSON
import Text.XML.HXT.Arrow.Pickle
import Data.Map (Map)
import qualified Data.Map as M

-------------------------------------------------------------------------------

deriveAll ''Map "PFMap"
type instance PF (Map a b) = PFMap a b

instance (XmlPickler a, XmlPickler b) => XmlPickler (Map a b) where
  xpickle = xpElem "map" (xpWrap (M.fromList, M.toList) (xpList (xpPair xpickle xpickle)))

instance (IsString a, JSON b) => JSON (Map a b) where
  showJSON = showJSON . toJSObject . map (first key) . unMap
  readJSON = fmap (Map . map (first Key) . fromJSObject) . readJSON

instance JSONSchema b => JSONSchema (Map Key b) where
  schema _ = field "key" False (schema (Proxy :: Proxy b))

instance Json b => Json (Map Key b)

