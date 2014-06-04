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
    showExample' s = case s of
      Choice []     -> A.Null -- Cannot create zero value
      Choice (x:_)  -> showExample' x
      Object fs     -> A.object $ map (\f -> pack (key f) .= showExample' (content f)) fs
      Map v         -> A.object ["<key>" .= showExample' v]
      Tuple vs      -> A.Array $ V.fromList $ map showExample' vs
      Array l _ _ v -> A.Array $ V.fromList $ replicate (l `max` 1) (showExample' v)
      Value _ _     -> A.String "value"
      Boolean       -> A.Bool True
      Number l _    -> A.Number (fromIntegral l)
      Null          -> A.Null
      Any           -> A.String "<value>"
