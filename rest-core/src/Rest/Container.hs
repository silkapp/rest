{-# LANGUAGE
    TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  , FlexibleInstances
  , ScopedTypeVariables
  , DeriveDataTypeable
  , GADTs
  #-}
module Rest.Container
  ( module Rest.Types.Container
  , listI
  , listO
  , mappingO
  , mappingI
  , statusO
  , reasonE
  ) where

import Data.String
import Data.String.ToString
import Data.Typeable

import Rest.Dictionary
import Rest.Error
import Rest.Types.Container


listI :: Inputs a -> Inputs (List a)
listI []          = []
listI (XmlI  : r) = XmlI  : listI r
listI (JsonI : r) = JsonI : listI r
listI (_     : r) = listI r

listO :: Outputs a -> Outputs (List a)
listO []          = []
listO (XmlO  : r) = XmlO  : listO r
listO (JsonO : r) = JsonO : listO r
listO (_     : r) = listO r

mappingO :: (Typeable k, IsString k, ToString k) => Outputs o -> Outputs (Map k o)
mappingO []          = []
mappingO (XmlO  : r) = XmlO  : mappingO r
mappingO (JsonO : r) = JsonO : mappingO r
mappingO (_     : r) = mappingO r

mappingI :: (Typeable k, IsString k, ToString k) => Inputs i -> Inputs (Map k i)
mappingI []          = []
mappingI (XmlI  : r) = XmlI  : mappingI r
mappingI (JsonI : r) = JsonI : mappingI r
mappingI (_     : r) = mappingI r

statusO :: Outputs o -> Errors e -> Outputs (Status e o)
statusO []          _  = []
statusO (XmlO  : r) es = concatMap (\v -> case v of { XmlE -> [XmlO]  ; NoE -> [XmlO] ; _ -> []}) es ++ statusO r es
statusO (JsonO : r) es = concatMap (\v -> case v of { JsonE -> [JsonO]; NoE -> [JsonO]; _ -> []}) es ++ statusO r es
statusO (_     : r) es = statusO r es

reasonE :: Errors a -> Errors (Reason a)
reasonE []          = []
reasonE (XmlE  : r) = XmlE  : reasonE r
reasonE (JsonE : r) = JsonE : reasonE r
reasonE (_     : r) = reasonE r

