{-# LANGUAGE
    DataKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , GADTs
  , ScopedTypeVariables
  , StandaloneDeriving
  , DeriveDataTypeable
  , RecordPuns
  #-}
module Rest.Format.Json (jsonI, jsonO, jsonE, json) where

import Data.Aeson (encode, eitherDecode, FromJSON, ToJSON, decode, Value)
import Data.ByteString.Lazy as ByteString
import Data.JSON.Schema
import Data.Maybe (fromJust)
import Data.Typeable
import Network.Multipart.Header
import qualified Data.Label.Total as L

import Rest.Dictionary.Types
import Rest.Types.Error
import Rest.Types.Container

input :: FromJSON i => Input i
input = Input
  { parser  = eitherDecode
  , accepts = ctOk
  }

-- TODO: json schema and typeable for docs?

jsonI :: (Typeable i, FromJSON i, JSONSchema i, FromMaybe i i' ~ i) => Dict h p i' o e -> Dict h p (Just i) o e
jsonI = L.modify inputs (modDicts (input:))

out :: forall o. (ToJSON o, Typeable o) => Output o
out = Output
  { printer = encode
  , returns = ctOk . fst
  , custom = JsonO :: JsonO o
  }

data JsonO o where
  JsonO :: ToJSON o => JsonO o

deriving instance Typeable JsonO

-- TODO: json schema and typeable for docs?

jsonO :: (Typeable o, ToJSON o, JSONSchema o, FromMaybe o o' ~ o) => Dict h p i o' e -> Dict h p i (Just o) e
jsonO = L.modify outputs (modDicts (out:))

err :: (Typeable e, ToJSON e, ToResponseCode e) => Error e
err = Error
  { output       = out
  , responseCode = toResponseCode
  }

-- TODO: json schema and typeable for docs?

jsonE :: (ToResponseCode e, Typeable e, ToJSON e, JSONSchema e, FromMaybe e e' ~ e) => Dict h p i o e' -> Dict h p i o (Just e)
jsonE = L.modify errors (modDicts (err:))

json :: ( Typeable i, Typeable o
        , FromJSON i, ToJSON o
        , JSONSchema i, JSONSchema o
        , FromMaybe i i' ~ i, FromMaybe o o' ~ o)
     => Dict h p i'       o'       e
     -> Dict h p (Just i) (Just o) e
json = jsonI . jsonO

ctOk :: ContentType -> Bool
ctOk ct = (ctType ct == "text" || ctType ct == "application")
       && ctSubtype ct == "json"

data ContainerDicts = ContainerDicts
  { mkReason :: forall e. Error e  -> Error (Reason e)
  , mkList   :: forall o. Output o -> Output (List o)
  }

containers :: ContainerDicts
containers = ContainerDicts
  { mkReason = \e -> _e
  , mkList   = \(Output{custom} :: Output o) -> case cast custom of
     Nothing -> undefined
     Just (JsonO :: JsonO o) -> out
  }

