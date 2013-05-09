module Rest.Gen.Utils where

import Data.Char
import Data.List.Split

import Paths_rest_gen

import System.FilePath
import Text.StringTemplate

readContent :: String -> IO String
readContent f = getDataFileName f >>= readFile

copyContent :: ToSElem a => [(String, a)] -> String -> String -> IO ()
copyContent ats f t = readContent f >>= return . render . setManyAttrib ats . newSTMP >>= writeFile (t </> last (splitOn "/" f))

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
