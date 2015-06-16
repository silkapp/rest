module Rest.Gen.Utils
  ( readContent
  , groupByFirst
  , fst3
  , snd3
  , thd3
  , upFirst
  , downFirst
  , mapHead
  , setupTargetDir
  ) where

import Prelude hiding (foldr)

import Data.Char
import Data.Foldable
import Data.List.Split
import System.Directory
import System.FilePath
import System.Process
import Text.StringTemplate

import Paths_rest_gen (getDataFileName)

readContent :: String -> IO String
readContent f = getDataFileName f >>= readFile

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

setupTargetDir :: Maybe FilePath -> FilePath -> IO ()
setupTargetDir msource targetDir = do
  createDirectoryIfMissing True targetDir
  forM_ msource $ \source -> system $ "cp -rf " ++ source ++ " " ++ targetDir
