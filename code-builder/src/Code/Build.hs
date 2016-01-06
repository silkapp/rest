{-# LANGUAGE
    CPP
  , FlexibleInstances
  , TypeSynonymInstances
  , UndecidableInstances
  #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
module Code.Build where

import Data.List

-- | Representation of code, each string represents a line
newtype Code = Code { unCode :: [String] }

showCode :: Code -> String
showCode = intercalate "\n" . unCode

-- | Type class for lifting data structures into code
class Codeable a where
  code :: a -> Code

instance Codeable Code where
  code = id

instance {-# OVERLAPPING #-} Codeable String where
  code = Code . (:[])

instance Codeable a => Codeable (Maybe a) where
  code = maybe noCode code

instance {-# OVERLAPPABLE #-} Show a => Codeable a where
  code = Code . (:[]) . show

class CodeList a where
  codeList :: a -> [Code]

instance {-# OVERLAPPABLE #-} Codeable a => CodeList a where
  codeList = (:[]) . code

instance {-# OVERLAPPING #-} Codeable a => CodeList [a] where
  codeList = map code

-- * Functions on code
noCode :: Code
noCode = Code []

line :: Code
line = Code [""]

isNull :: Codeable a => a -> Bool
isNull = null . unCode . code

numLines :: CodeList a => a -> Int
numLines = length . unCode . mkStack . codeList

singleLine :: CodeList a => a -> Bool
singleLine = (==1) . numLines

indent :: Codeable a => Int -> a -> Code
indent n = Code . map (replicate n ' ' ++) . unCode . code

surround :: Codeable a => String -> String -> a -> Code
surround l r a = l <+> a <+> r

parenthesis :: Codeable a => a -> Code
parenthesis = surround "(" ")"

accolades :: Codeable a => a -> Code
accolades = surround "{" "}"

square :: Codeable a => a -> Code
square = surround "[" "]"

align :: Codeable a => a -> Code
align v = Code . map addWhite . unCode . code $ v
    where addWhite l = l ++ replicate (codeWidth v - length l) ' '

codeWidth :: Codeable a => a -> Int
codeWidth = foldr max 0 . map length . unCode . code

codeLines :: Codeable a => a -> [Code]
codeLines = map (Code . (:[])) . unCode . code

many :: Codeable a => a -> Code
many = Code . concat . repeat . unCode . code

mkSequence :: Codeable a => [a] -> Code
mkSequence = foldl (<+>) noCode . map code

mkStack :: Codeable a => [a] -> Code
mkStack = foldl (<->) noCode . map code

interleave :: (Codeable a, CodeList l) => a -> l -> Code
interleave c l =
  case codeList l of
    []      -> noCode
    [x]     -> x
    (x: xs) -> x <+> code c <+> interleave c xs

-- * Combinators for building blocks of code
infixl 4 <+>
infixl 4 <++>
infixl 4 <+|

infixl 3 |>+<|
infixl 3 |><|
infixl 3 ><
infixl 3 |><
infixl 3 ><|

infixl 2 <->

-- | Join two blocks line by line, in the way of inner join, so both lines have to be present.
(|><|) :: (Codeable a, Codeable b) => a -> b -> Code
a |><| b = Code $ zipWith (++) ca cb
        where ca = unCode . code $ a
              cb = unCode . code $ b

-- | Join two blocks line by line, in the way of outer join, so both missing lines are discarded.
(><) :: (Codeable a, Codeable b) => a -> b -> Code
a >< b = a <-> mkStack (replicate ((numLines (code b) - numLines (code a)) `max` 0) line) |><| b <-> mkStack (replicate ((numLines (code a) - numLines (code b)) `max` 0) line)

-- | Left outer-join
(|><) :: (Codeable a, Codeable b) => a -> b -> Code
a |>< b = a <-> mkStack (replicate ((numLines (code b) - numLines (code a)) `max` 0) line) |><| b

-- | Right outer-join
(><|) :: (Codeable a, Codeable b) => a -> b -> Code
a ><| b = a |><| b <-> mkStack (replicate ((numLines (code a) - numLines (code b)) `max` 0) line)

-- | Sequencing. Place the second block after the last line of the first block. Aligns the second block
(<+>) :: (Codeable a, Codeable b) => a -> b -> Code
a <+> b =
  case ca of
    [] -> code b
    ls -> case cb of
            []        -> code a
            (bl: bls) -> Code $ init ls ++ [last ls ++ bl] ++ bls
  where ca = unCode . code $ a
        cb = unCode . code $ b

-- | Same as <++> but with space
(<++>) :: (Codeable a, Codeable b) => a -> b -> Code
a <++> b
  | empty a = code b
  | empty b = code a
  | otherwise = a <+> " " <+> b
    where
     empty x = all (== "") (unCode (code x))

-- | Place the second block after the last line of the first block. Aligns the second block
(<+|) :: (Codeable a, Codeable b) => a -> b -> Code
a <+| b =
  case ca of
    [] -> code b
    ls -> case cb of
            []        -> code a
            (bl: bls) -> Code $ init ls ++ [last ls ++ bl] ++ map (replicate (length $ last ls) ' ' ++) bls
  where ca = unCode . code $ a
        cb = unCode . code $ b

-- | Combination of join and sequence. The code blocks in the second argument are sequenced with the first argument.
(|>+<|) :: (Codeable a, CodeList b) => a -> b -> Code
a |>+<| b = mkStack $ zipWith (<+|) (codeLines a) (codeList b)

-- | Place two pieces of code under each other
(<->) :: (Codeable a, Codeable b) => a -> b -> Code
a <-> b = Code $ (unCode $ code a) ++ (unCode $ code b)

