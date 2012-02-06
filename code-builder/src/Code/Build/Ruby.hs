module Code.Build.Ruby where

import Code.Build
import Data.List

hashmap :: [(String, Code)] -> Code
hashmap [] = code "{}"
hashmap items = ("{ " <-> many ", ") |><| mkStack (map ((':': ) . fst) items) |><| many " => " |>+<| map snd items <-> "}"

statements :: CodeList a => a -> Code
statements = mkStack . codeList

block :: CodeList a => a -> Code
block c = indent 2 (statements c)

endBlock :: CodeList a => a -> Code
endBlock c = indent 2 (statements c) <-> "end"

rbIf :: (Codeable a, CodeList b) => a -> b -> Code
rbIf cond blk = "if" <++> parenthesis cond <-> endBlock blk

rbIf_ :: (Codeable a, CodeList b) => a -> b -> Code
rbIf_ cond blk = "if" <++> parenthesis cond <-> block blk

rbElsIf :: (Codeable a, CodeList b) => a -> b -> Code
rbElsIf cond blk = "elsif" <++> parenthesis cond <-> endBlock blk

rbElsIf_ :: (Codeable a, CodeList b) => a -> b -> Code
rbElsIf_ cond blk = "elsif" <++> parenthesis cond <-> block blk

rbElse :: CodeList b => b -> Code
rbElse blk = "else" <-> endBlock blk

--rbIfElse :: CodeList a => a -> Code
--rbIfElse blk = "else" <-> block blk

rbModule :: CodeList a => String -> a -> Code
rbModule name c = "module" <++> name <-> endBlock c

rbClass :: CodeList a => String -> a -> Code
rbClass name c = "class" <++> name <-> endBlock c

require :: String -> Code
require name = "require" <++> string name

function :: CodeList a => String -> [String] -> a -> Code
function name pars body = "def" <++> name <++> parenthesis (intercalate ", " pars) <-> endBlock body

call :: CodeList a => String -> a -> Code
call func as | all singleLine (codeList as) = func <+> parenthesis (interleave ", " $ codeList as)
             | otherwise                    = func <+| (("( " <-> many ", ") |>+<| as) <+> ")"

ret :: Codeable a => a -> Code
ret a = "return " <+| a

string :: Codeable a => a -> Code
string = surround "'" "'"

new :: CodeList a => String -> a ->  Code
new clas pars = clas ++ ".new" <+| parenthesis (interleave ", " $ codeList pars)

infix 2 .=.

(.=.) :: Codeable b => String -> b -> Code
v .=. c | singleLine (code c) = v <+> " = " <+| c
        | otherwise           = v <+> " =" <-> indent 2 c