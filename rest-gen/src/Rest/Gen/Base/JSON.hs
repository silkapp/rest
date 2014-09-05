{-# LANGUAGE
    CPP
  , OverloadedStrings
  #-}
module Rest.Gen.Base.JSON (showExample) where

import Data.Aeson ((.=))
import Data.JSON.Schema
import Data.Maybe
import Text.PrettyPrint.HughesPJ
import qualified Data.Aeson  as A
import qualified Data.Vector as V

import Rest.Gen.Base.JSON.Pretty

showExample :: Schema -> String
showExample = render . pp_value . showExample'
  where
    showExample' s = case s of
      Choice []    -> A.Null -- Cannot create zero value
      Choice (x:_) -> showExample' x
      Object fs    -> A.object $ map (\f -> key f .= showExample' (content f)) fs
      Map v        -> A.object ["<key>" .= showExample' v]
      Tuple vs     -> A.Array $ V.fromList $ map showExample' vs
      Array l _ v  -> A.Array $ V.fromList $ replicate (lengthBoundExample l `max` 1) (showExample' v)
      Value _      -> A.String "value"
      Boolean      -> A.Bool True
      Number b     -> A.Number . boundExample $ b
      Any          -> A.String "<value>"
      Constant v   -> v
#if !MIN_VERSION_json_schema(0,7,0)
      Null         -> A.Null
#endif

boundExample :: Num a => Bound -> a
boundExample b = fromIntegral $ fromMaybe 0 (maybe (lower b) Just (upper b))

lengthBoundExample :: Num a => LengthBound -> a
lengthBoundExample b = fromIntegral $ fromMaybe 0 (maybe (lowerLength b) Just (upperLength b))
