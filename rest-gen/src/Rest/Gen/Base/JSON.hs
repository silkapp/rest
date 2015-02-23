{-# LANGUAGE
    CPP
  , OverloadedStrings
  #-}
module Rest.Gen.Base.JSON
  ( showExample
  , showExamples
  ) where

import Data.Aeson ((.=))
import Data.JSON.Schema
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe
import Text.PrettyPrint.HughesPJ
import qualified Data.Aeson  as A
import qualified Data.Text   as T
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


-- | Generate enough examples as to mention every possible field
--   at least once
showExamples :: Schema -> [String]
showExamples = map (render . pp_value) . showExamples'
  where
    -- Recursive types have infinite schemas, so we must ensure
    -- that we don't generate infinitely deep examples in that case.
    -- We thus impose a limit to the number of nestings
    showExamples' = fst . go (10 :: Int)

    -- Idea: @go n x == (showExamples' n x, length (showExamples' n x))@
    -- and note that @snd (go x) >= 1@
    go 0 _ = ([A.String "..."],1)
    go n s = case s of
      Choice []   -> ([A.Null], 1)

      Choice xs   -> let (examples, numExamples) = unzip $ map (go $ n-1) xs
                     in (concat examples, sum numExamples)

      Object []   -> ([A.object []], 1)

      Object fs   -> let (esByFld, numEsByFld) = unzip $ map (fieldExs n) fs
                         numExamples = maximum numEsByFld
                         examples = map A.object $ take numExamples $ transpose $ map cycle esByFld
                     in (examples, numExamples)

      Map v       -> let (examples, _) = go (n-1) v
                         mkKey i = T.pack $ "<key " ++ show (i :: Int) ++ ">"
                     in ([A.object [mkKey i .= e | (i,e) <- zip [1..] examples]], 1)

      Tuple []    -> ([A.Array V.empty], 1)

      Tuple vs    -> let (esByPos,numEsByPos) = unzip $ map (go $ n-1) vs
                         numExamples = maximum numEsByPos
                         examples = take numExamples $ transpose $ map cycle esByPos
                     in (map (A.Array . V.fromList) examples, numExamples)

      Array l _ v -> let minLen = fromMaybe 0 (lowerLength l)
                         maxLen = fromMaybe (numCases `max` minLen) (upperLength l)
                         (cases,numCases) = go (n-1) v
                         (q,r) = numCases `divMod` maxLen
                         numExamples = q + signum r
                         examples = chunksOf maxLen $ cases ++ take (minLen - r) cases
                     in if maxLen == 0 then ([A.Array V.empty], 1)
                        else (map (A.Array . V.fromList) examples, numExamples)

      Value _     -> ([A.String "value"], 1)

      Boolean     -> ([A.Bool True], 1)

      Number b    -> ([A.Number $ boundExample b], 1)

      Any         -> ([A.String "<value>"],1)

      Constant v  -> ([v], 1)

#if !MIN_VERSION_json_schema(0,7,0)
      Null        -> ([A.Null], 1)
#endif

    fieldExs n f = let (examples,num_examples) = go (n-1) (content f)
                   in (map (key f .=) examples, num_examples)


boundExample :: Num a => Bound -> a
boundExample b = fromIntegral $ fromMaybe 0 (maybe (lower b) Just (upper b))

lengthBoundExample :: Num a => LengthBound -> a
lengthBoundExample b = fromIntegral $ fromMaybe 0 (maybe (lowerLength b) Just (upperLength b))
