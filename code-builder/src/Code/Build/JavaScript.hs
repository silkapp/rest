module Code.Build.JavaScript where

import Code.Build
import Data.List

jsObject :: [(String, Code)] -> Code
jsObject items = ("{ " <-> many ", ") |><| mkStack (map fst items) |><| many ": " |>+<| map snd items <-> "}"

block :: CodeList a => a -> Code
block c = "{" <-> indent 2 (mkStack $ map (<+> ";") $ codeList c) <-> "}"

iff :: (Codeable a, CodeList b) => a -> b -> Code
iff cond blk = "if" <++> parenthesis cond <++> block blk

function :: CodeList a => [String] -> a -> Code
function pars body = "function" <++> parenthesis (intercalate ", " pars) <++> block body

call :: CodeList a => String -> a -> Code
call func as | singleLine as = func <++> parenthesis (interleave "," $ codeList as)
             | otherwise     = func <+| ((" (" <-> many " ,") |>+<| as) <+> ")"

proc :: CodeList a => String -> a -> Code
proc f a = call f a

string :: Codeable a => a -> Code
string = surround "\"" "\""

var :: Codeable a => String -> a -> Code
var n c | singleLine (code c) = "var " <+> n <+> " = " <+| c <+> ";"
        | otherwise           = "var " <+> n <+> " =" <-> indent 2 c <+> ";"