module Code.Build.JavaScript where

import Code.Build
import Data.List

jsObject :: [(String, Code)] -> Code
jsObject [] = code "{}"
jsObject items = ("{ " <-> many ", ") |><| mkStack (map fst items) |><| many ": " |>+<| map snd items <-> "}"

statements :: CodeList a => a -> Code
statements = mkStack . codeList

block :: CodeList a => a -> Code
block c = "{" <-> indent 2 (statements c) <-> "}"

iff :: (Codeable a, CodeList b) => a -> b -> Code
iff cond blk = "if" <++> parenthesis cond <-> block blk

for :: (Codeable a, CodeList b) => a -> b -> Code
for cond blk = "for" <++> parenthesis cond <-> block blk

function :: CodeList a => [String] -> a -> Code
function pars body = "function" <++> parenthesis (intercalate ", " pars) <-> block body

functionDecl :: CodeList a => String -> [String] -> a -> Code
functionDecl name pars body = "function" <++> name <++> parenthesis (intercalate ", " pars) <-> block body

call :: CodeList a => String -> a -> Code
call func as | all singleLine (codeList as) = func <+> parenthesis (interleave ", " $ codeList as)
             | otherwise                    = func <+| (("( " <-> many ", ") |>+<| as) <+> ")"

proc :: CodeList a => String -> a -> Code
proc f a = call f a <+> ";"

ret :: Codeable a => a -> Code
ret a = "return " <+| a <+> ";"

string :: Codeable a => a -> Code
string = surround "\"" "\""

new :: Codeable a => String -> a ->  Code
new clas c = "new" <++> clas <+| parenthesis c

var :: Codeable a => String -> a -> Code
var n c | singleLine (code c) = "var " <+> n <+> " = " <+| c <+> ";"
        | otherwise           = "var " <+> n <+> " =" <-> indent 2 c <+> ";"

infix 2 .=.

(.=.) :: Codeable b => String -> b -> Code
v .=. c | singleLine (code c) = v <+> " = " <+| c <+> ";"
        | otherwise           = v <+> " =" <-> indent 2 c <+> ";"