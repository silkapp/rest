{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  #-}
module Api.Test
  ( resource
  , WithText
  , Err (..)
  , Ok (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Char (isSpace)
import Data.Data
import Data.JSON.Schema
import Data.List (nub)
import Data.List.Split
import Data.Text (Text)
import GHC.Generics
import Generics.Generic.Aeson
import Generics.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Rest
import Rest.Dictionary (Format (..), Header (..))
import qualified Rest.Resource as R

import ApiTypes
import qualified Api.Test.Err2 as E2

-- | User extends the root of the API with a reader containing the ways to identify a user in our URLs.
-- Currently only by the user name.
type WithText = ReaderT Text BlogApi

data Err = Err deriving (Generic, Show, Typeable)
instance ToJSON     Err where toJSON    = gtoJson
instance FromJSON   Err where parseJSON = gparseJson
instance JSONSchema Err where schema    = gSchema
instance XmlPickler Err where xpickle   = gxpickle

instance ToResponseCode Err where
  toResponseCode _ = 400

data Ok = Ok deriving (Generic, Show, Typeable)
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
                , ("rawXmlIO"           , rawXmlIO           )
                , ("rawJsonIO"          , rawJsonIO          )
                , ("rawJsonAndXmlIO"    , rawJsonAndXmlIO    )
                , ("noError"            , noError            )
                , ("justStringO"        , justStringO        )
                , ("preferJson"         , preferJson         )
                , ("octetStreamOut"     , octetStreamOut     )
                , ("onlyInput"          , onlyInput          )
                ]
  }

noResponse :: Handler WithText
noResponse = mkConstHandler id $ return ()

onlyError :: Handler WithText
onlyError = mkConstHandler jsonE $
  throwError $ domainReason Err

differentFormats :: Handler WithText
differentFormats = mkInputHandler (jsonE . xmlO . stringI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

intersectedFormats :: Handler WithText
intersectedFormats = mkInputHandler (jsonE . xmlO . jsonO . stringI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

intersectedFormats2 :: Handler WithText
intersectedFormats2 = mkInputHandler (xmlE . xmlO . jsonO . stringI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

rawXmlIO :: Handler WithText
rawXmlIO = mkIdHandler (rawXmlI . rawXmlO . xmlE) $ \s _ ->
  case s of
    "<error/>" -> throwError $ domainReason E2.Err
    _          -> return "<ok/>"

rawJsonIO :: Handler WithText
rawJsonIO = mkIdHandler (rawJsonI . rawJsonO . jsonE) $ \s _ ->
  case s of
    "\"error\"" -> throwError $ domainReason E2.Err
    _           -> return "\"ok\""

rawJsonAndXmlIO :: Handler WithText
rawJsonAndXmlIO = mkHandler (mkHeader accept . rawJsonAndXmlI . rawJsonAndXmlO) handler
  where
    handler :: Env (Maybe Format) () (Either Json Xml) -> ExceptT Reason_ WithText ByteString
    handler = \case
       Env (Just JsonFormat) _ (Left (Json _)) -> return "\"jsonInput\""
       Env (Just JsonFormat) _ (Right (Xml _)) -> return "\"xmlInput\""
       Env (Just XmlFormat ) _ (Left (Json _)) -> return "<jsonInput/>"
       Env (Just XmlFormat ) _ (Right (Xml _)) -> return "<xmlInput/>"
       Env _ _ _ -> throwError $ OutputError $ UnsupportedFormat "Only json and xml accept headers are allowed"

accept :: Header (Maybe Format)
accept = Header ["Accept"] $ \xs ->
  let formats = concatMap parseAccept xs
      jsonp   = JsonFormat `elem` formats
      xmlp    = XmlFormat  `elem` formats
  in return $
       if jsonp
         then Just JsonFormat
         else if xmlp
           then Just XmlFormat
           else Nothing
  where
    -- Adapted accept header parsing from rest-core, this doesn't take Content-Type into account.
    parseAccept :: Maybe String -> [Format]
    parseAccept acceptHeader =
      maybe allFormats splitter acceptHeader
      where
        allFormats :: [Format]
        allFormats = [minBound .. maxBound]
        splitter :: String -> [Format]
        splitter hdr = nub (match =<< takeWhile (/= ';') . trim <$> splitOn "," hdr)

        match :: [Char] -> [Format]
        match ty =
          case map trim <$> (splitOn "+" . trim <$> splitOn "/" ty) of
            [ ["*"]           , ["*"] ] -> allFormats
            [ ["*"]                   ] -> allFormats
            [ ["text"]        , xs    ] -> xs >>= txt
            [ ["application"] , xs    ] -> xs >>= app
            [ ["image"]       , xs    ] -> xs >>= img
            _                           -> []

        trim :: String -> String
        trim = f . f
          where f = reverse . dropWhile isSpace

        txt :: String -> [Format]
        txt "*"            = [XmlFormat, JsonFormat, StringFormat]
        txt "json"         = [JsonFormat]
        txt "xml"          = [XmlFormat]
        txt "plain"        = [StringFormat]
        txt _              = []
        app :: String -> [Format]
        app "*"            = [XmlFormat, JsonFormat, FileFormat]
        app "xml"          = [XmlFormat]
        app "json"         = [JsonFormat]
        app "octet-stream" = [FileFormat]
        app _              = []
        img :: String -> [Format]
        img _              = [FileFormat]

noError :: Handler WithText
noError = mkConstHandler jsonO $ return Ok

justStringO :: Handler WithText
justStringO = mkConstHandler stringO $ return "Ok"

preferJson :: Handler WithText
preferJson = mkInputHandler (xmlJsonO . xmlJsonE . stringI) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return Ok

octetStreamOut :: Handler WithText
octetStreamOut = mkInputHandler (fileI . fileO . xmlJsonE) $
  \case
    "error" -> throwError $ domainReason Err
    _       -> return ("ok", "ok", False)

onlyInput :: Handler WithText
onlyInput = mkInputHandler jsonI $ \() -> throwError NotFound
