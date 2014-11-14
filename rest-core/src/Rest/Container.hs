{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleInstances
  , GADTs
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  #-}
module Rest.Container
  ( module Rest.Types.Container
  , listI
  , listO
  , mappingI
  , mappingO
  , statusO
  , reasonE
  , defaultE
  ) where

import Data.Maybe

import Rest.Dictionary
import Rest.Error
import Rest.StringMap.HashMap.Strict
import Rest.Types.Container

listI :: Inputs a -> Maybe (Inputs (List a))
listI None       = Just (Dicts [XmlI, JsonI])
listI (Dicts is) =
  case mapMaybe listDictI is of
    []  -> Nothing
    lis -> Just (Dicts lis)
  where
    listDictI :: Input a -> Maybe (Input (List a))
    listDictI XmlI  = Just XmlI
    listDictI JsonI = Just JsonI
    listDictI _     = Nothing

listO :: Outputs a -> Maybe (Outputs (List a))
listO None       = Just (Dicts [XmlO, JsonO])
listO (Dicts os) =
  case mapMaybe listDictO os of
    []  -> Nothing
    los -> Just (Dicts los)
  where
    listDictO :: Output a -> Maybe (Output (List a))
    listDictO XmlO  = Just XmlO
    listDictO JsonO = Just JsonO
    listDictO _     = Nothing

mappingI :: forall i. Inputs i -> Maybe (Inputs (StringHashMap String i))
mappingI None       = Just (Dicts [XmlI, JsonI])
mappingI (Dicts is) =
  case mapMaybe mappingDictI is of
    []  -> Nothing
    mis -> Just (Dicts mis)
  where
    mappingDictI :: Input i -> Maybe (Input (StringHashMap String i))
    mappingDictI XmlI  = Just XmlI
    mappingDictI JsonI = Just JsonI
    mappingDictI _     = Nothing

mappingO :: forall o. Outputs o -> Maybe (Outputs (StringHashMap String o))
mappingO None       = Just (Dicts [XmlO, JsonO])
mappingO (Dicts os) =
  case mapMaybe mappingDictO os of
    []  -> Nothing
    mos -> Just (Dicts mos)
  where
    mappingDictO :: Output o -> Maybe (Output (StringHashMap String o))
    mappingDictO XmlO  = Just XmlO
    mappingDictO JsonO = Just JsonO
    mappingDictO _     = Nothing

statusO :: Errors e -> Outputs o -> Maybe (Outputs (Status e o))
statusO None       None       = Just (Dicts [XmlO, JsonO])
statusO None       (Dicts os) = mkStatusDict [XmlE, JsonE] os
statusO (Dicts es) None       = mkStatusDict es           [XmlO, JsonO]
statusO (Dicts es) (Dicts os) = mkStatusDict es           os

mkStatusDict :: forall e o. [Error e] -> [Output o] -> Maybe (Outputs (Status e o))
mkStatusDict es os =
    case mapMaybe mappingDictO (intersect es os) of
      []  -> Nothing
      sos -> Just (Dicts sos)
    where
      mappingDictO :: (Error e, Output o) -> Maybe (Output (Status e o))
      mappingDictO (XmlE , XmlO ) = Just XmlO
      mappingDictO (JsonE, JsonO) = Just JsonO
      mappingDictO _              = Nothing

intersect :: [Error e] -> [Output o] -> [(Error e, Output o)]
intersect [] _  = []
intersect _  [] = []
intersect es os = [ (e, o) | e <- es, o <- os, e `eq` o ]
  where
    XmlE  `eq` XmlO  = True
    JsonE `eq` JsonO = True
    _     `eq` _     = False

reasonE :: Errors a -> Errors (Reason a)
reasonE None       = Dicts [XmlE, JsonE]
reasonE (Dicts es) = Dicts (map reasonDictE es)
  where
    reasonDictE :: Error a -> Error (Reason a)
    reasonDictE XmlE  = XmlE
    reasonDictE JsonE = JsonE

defaultE :: Errors ()
defaultE = Dicts [XmlE, JsonE]
