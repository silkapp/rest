{-# LANGUAGE OverloadedStrings #-}
module Rest.Types.Method (Method (..)) where

import Control.Applicative
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import Data.JSON.Schema (JSONSchema (..))
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml

import qualified Data.Aeson       as Json
import qualified Data.JSON.Schema as Schema
import qualified Data.Text        as Text

data Method = GET | PUT | POST | DELETE
  deriving (Show, Eq, Bounded, Enum)

instance ToJSON     Method where
  toJSON = toJSON . methodToString

instance FromJSON   Method where
  parseJSON (Json.String s) = case s of
    "GET"    -> return GET
    "PUT"    -> return PUT
    "POST"   -> return POST
    "DELETE" -> return DELETE
    m -> fail $ "Unknown string when parsing method: " ++ Text.unpack m
  parseJSON j = typeMismatch "String" j

instance JSONSchema Method where
  schema _ = Schema.Choice [ Schema.Constant (Json.String "GET")
                           , Schema.Constant (Json.String "PUT")
                           , Schema.Constant (Json.String "POST")
                           , Schema.Constant (Json.String "DELETE")
                           ]

instance XmlPickler Method where
  xpickle = PU
    (\x -> appPickle (xpElem (methodToStringLC x) (xpickle :: PU ())) ())
    (choices (map mkUnpickler enumAll))
    (scAlts (map (\m -> scElem (methodToStringLC m) scEmpty) enumAll))

mkUnpickler :: Method -> Unpickler Method
mkUnpickler m = appUnPickle (xpWrap (const m, const ())
                              (xpElem (methodToStringLC m)
                                (xpickle :: PU ()))
                            )

choice :: Unpickler a -> Unpickler a -> Unpickler a
choice x y = mchoice x pure y

choices :: [Unpickler a] -> Unpickler a
choices = foldr1 choice

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

methodToString :: Method -> String
methodToString GET    = "GET"
methodToString PUT    = "PUT"
methodToString POST   = "POST"
methodToString DELETE = "DELETE"

methodToStringLC :: Method -> String
methodToStringLC = map toLower . methodToString
