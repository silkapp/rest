module Rest.Gen.Base.JSON.Pretty (pp_value) where

import Prelude hiding ((<>))

import Control.Arrow (first)
import Data.Aeson.Types
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text, unpack)
import Numeric
import Text.PrettyPrint.HughesPJ hiding (first)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector         as V

{-# ANN module "Hlint: ignore Use camelCase" #-}

pp_value         :: Value -> Doc
pp_value v        = case v of
    Null      -> pp_null
    Bool x    -> pp_boolean x
    Number x  -> pp_number . either Right Left . floatingOrInteger $ x
    String x  -> pp_js_string (unpack x)
    Array vs  -> pp_array $ V.toList vs
    Object xs -> pp_js_object xs

pp_null          :: Doc
pp_null           = text "null"

pp_boolean       :: Bool -> Doc
pp_boolean True   = text "true"
pp_boolean False  = text "false"

pp_number        :: Either Integer Double -> Doc
pp_number        = either integer double

pp_array         :: [Value] -> Doc
pp_array xs       = vlist "[" "]" $ map pp_value xs

vlist :: String -> String -> [Doc] -> Doc
vlist o c [] = text o <+> text c
vlist o c ls = vcat $ text o <+> head ls : map (comma <+>) (tail ls) ++ [text c]

pp_string        :: String -> Doc
pp_string x       = doubleQuotes $ hcat $ map pp_char x
  where pp_char '\\'            = text "\\\\"
        pp_char '"'             = text "\\\""
        pp_char c | isControl c || fromEnum c >= 0x7f = uni_esc c
        pp_char c               = char c

        uni_esc c = text "\\u" <> text (pad 4 (showHex (fromEnum c) ""))

        pad n cs  | len < n   = replicate (n-len) '0' ++ cs
                  | otherwise = cs
          where len = length cs

pp_object        :: [(String,Value)] -> Doc
pp_object xs      = vlist "{" "}" $ map pp_field xs
  where pp_field (k,v) = pp_string k <> colon <+> pp_value v

pp_js_string     :: String -> Doc
pp_js_string      = pp_string

pp_js_object     :: HashMap Text Value -> Doc
pp_js_object      = pp_object . map (first unpack) . H.toList
