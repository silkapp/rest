{-# LANGUAGE OverloadedStrings #-}
module Rest.Gen.Base.JSON (showExample) where

import Data.Aeson ((.=))
import Data.JSON.Schema
import Data.Text (pack)
import Text.PrettyPrint.HughesPJ
import qualified Data.Aeson  as A
import qualified Data.Vector as V

import Rest.Gen.Base.JSON.Pretty

showExample :: Schema -> String
showExample = render . pp_value . showExample'
  where
    showExample' (Choice [])     = A.Null -- Cannot create zero value
    showExample' (Choice (x:_))  = showExample' x
    showExample' (Object fs)     = A.object $ map (\f -> pack (key f) .= showExample' (content f)) fs
    showExample' (Tuple vs)      = A.Array $ V.fromList $ map showExample' vs
    showExample' (Array l _ _ v) = A.Array $ V.fromList $ replicate (l `max` 1) (showExample' v)
    showExample' (Value _ _)     = A.String "value"
    showExample' Boolean         = A.Bool True
    showExample' (Number l _)    = A.Number (fromIntegral l)
    showExample' Null            = A.Null
