{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , LambdaCase
  , TemplateHaskell
  , TypeFamilies
  #-}
module Api.Test where

import Control.Monad.Reader
import Control.Monad.Trans.Error
import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.Text (Text)
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Rest
import qualified Rest.Resource as R

import ApiTypes

-- | User extends the root of the API with a reader containing the ways to identify a user in our URLs.
-- Currently only by the user name.
type WithText = ReaderT Text BlogApi

data Err = Err deriving (Generic, Show, Typeable)
deriveAll ''Err "PFErr"
type instance PF Err = PFErr
instance ToJSON     Err where toJSON    = gtoJson
instance FromJSON   Err where parseJSON = gparseJson
instance JSONSchema Err where schema    = gSchema
instance XmlPickler Err where xpickle   = gxpickle

instance ToResponseCode Err where
  toResponseCode _ = 400

data Ok = Ok deriving (Generic, Show, Typeable)
deriveAll ''Ok "PFOk"
type instance PF Ok = PFOk
instance XmlPickler Ok where xpickle = gxpickle
instance ToJSON     Ok where toJSON    = gtoJson
instance FromJSON   Ok where parseJSON = gparseJson
instance JSONSchema Ok where schema    = gSchema

resource :: Resource BlogApi WithText Text Void Void
resource = mkResourceReader
  { R.name    = "test"
  , R.actions = [ ("noResponse"         , noResponse         )
                , ("onlyError"          , onlyError          )
                , ("differentFormats"   , differentFormats   )
                , ("intersectedFormats" , intersectedFormats )
                , ("intersectedFormats2", intersectedFormats2)
                ]
  }

noResponse :: Handler WithText
noResponse = mkConstHandler id $ return ()

onlyError :: Handler WithText
onlyError = mkConstHandler (jsonE . someE) $
  throwError $ domainReason Err

differentFormats :: Handler WithText
differentFormats = mkInputHandler (jsonE . someE . xmlO . someO . stringI . someI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

intersectedFormats :: Handler WithText
intersectedFormats = mkInputHandler (jsonE . someE . xmlO . jsonO . someO . stringI . someI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

intersectedFormats2 :: Handler WithText
intersectedFormats2 = mkInputHandler (xmlE . someE . xmlO . jsonO . someO . stringI . someI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok
