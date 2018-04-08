{-# LANGUAGE TemplateHaskell #-}
module Rest.Gen.Utils
  ( groupByFirst
  , fst3
  , snd3
  , thd3
  , upFirst
  , downFirst
  , mapHead

  , jsEpilogue
  , jsPrelude
  , rbBase
  ) where

import Prelude.Compat

import Data.ByteString (ByteString)
import Data.Char
import Data.FileEmbed

jsEpilogue :: ByteString
jsEpilogue = $(embedFile "files/Javascript/epilogue.js")

jsPrelude :: ByteString
jsPrelude = $(embedFile "files/Javascript/prelude.js")

rbBase :: ByteString
rbBase = $(embedFile "files/Ruby/base.rb")

groupByFirst :: Eq a => [(a,b)] -> [(a,[b])]
groupByFirst = foldr add []
  where add (k, v) l =
          case lookup k l of
            Nothing -> (k,[v]) : l
            Just vs -> (k, v: vs) : filter ((/=k) . fst) l

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

upFirst :: String -> String
upFirst = mapHead toUpper

downFirst :: String -> String
downFirst = mapHead toLower

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
