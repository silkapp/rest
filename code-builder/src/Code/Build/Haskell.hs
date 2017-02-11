module Code.Build.Haskell where

import Code.Build
import Data.List

record :: [(String, Code)] -> Code
record []    = code "{}"
record items = ("{ " <-> many ", ") |><| mkStack (map fst items) |><| many " = " |>+<| map snd items <-> "}"

block :: CodeList a => a -> Code
block = mkStack . codeList

hsModule :: CodeList a => String -> a -> Code
hsModule name c = "module" <++> name <++> "where" <-> block c

hsType :: [String] -> String
hsType = intercalate " -> "

hsDecl :: Codeable a => String -> [String] -> a -> Code
hsDecl n pars c | singleLine (code c) = n <++> unwords pars <++> "= " <+| c
                | otherwise = n <++> unwords pars <++> "=" <-> indent 2 c

function :: String -> String -> Code
function n t = n <++> "::" <++> t

hsLet :: (CodeList a, Codeable b) => a -> b -> Code
hsLet decls f = ("let " <+| block decls) <-> "in " <+| code f

hsArray :: CodeList a => a -> Code
hsArray = surround "[" "]" . interleave ", "

hsTuple :: CodeList a => a -> Code
hsTuple = parenthesis . interleave ", "

hsData :: String -> [String] -> Code
hsData n cons = "data" <++> n <+| (" = " <-> many " | ") |><| block cons

string :: Codeable a => a -> Code
string = surround "\"" "\""

infix 2 .=.

(.=.) :: Codeable b => String -> b -> Code
v .=. c | singleLine (code c) = v <+> " = " <+| c
        | otherwise           = v <+> " =" <-> indent 2 c
