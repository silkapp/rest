module Rest.Gen.Base.Link
  ( Link
  , LinkItem (..)
  , flattenLast
  , flattenLastResource
  , flattenLink
  , getLinkIds
  , hasParam
  , itemString
  , setLinkIds
  ) where

import Prelude.Compat

-- | Data structure representing Api links
data LinkItem =
    LResource String
  | LAction String
  | LParam String
  | LAccess [Link] deriving Show

instance Eq LinkItem where
  (==) (LAccess a) (LAccess b) = a == b
  (==) (LAccess _) _ = False
  (==) _ (LAccess _) = False
  (==) a b = itemString a == itemString b

type Link = [LinkItem]

hasParam :: LinkItem -> Bool
hasParam (LAccess l) = any (any hasParam) l
hasParam (LParam _) = True
hasParam _ = False

itemString :: LinkItem -> String
itemString li =
  case li of
    LResource s   -> s
    LAction   s   -> s
    LParam    s   -> s
    _             -> ""

flattenLink :: Link -> [Link]
flattenLink []               = [[]]
flattenLink (LAccess ls: rs) = [l ++ r | l <- concatMap flattenLink ls, r <- flattenLink rs]
flattenLink (v: rs)          = map (v :) $ flattenLink rs

flattenFrom :: (LinkItem -> Bool) -> Link -> [Link]
flattenFrom f = (\(end, start) -> [ reverse start ++ epart | epart <- flattenLink (reverse end)]) . break f . reverse

flattenLast :: Link -> [Link]
flattenLast [] = [[]]
flattenLast xs = (\l -> map (reverse (tail l) ++) $ flattenLink [head l]) . reverse $ xs

flattenLastResource :: Link -> [Link]
flattenLastResource = flattenFrom $ \x -> case x of LResource _ -> True; _ -> False
{-
-- | Make link by replacing identifiers with data
mkLink :: Link -> [String] -> String
mkLink []                     _        = ""
mkLink ((LAction s) : ls)     ps       = "/" ++ s ++ mkLink ls ps
mkLink ((LAccess _) : ls)     (p : ps) = "/" ++ p ++ mkLink ls ps

-- | Make link by replacing the identifying parts
mkLink' :: Link -> [String] -> String
mkLink' []                     _            = ""
mkLink' ((LAction s) : ls)     ps           = "/" ++ s ++ mkLink' ls ps
mkLink' ((LAccess _) : ls)     (p : ps)     = "/" ++ p ++ mkLink' ls ps
-}

getLinkIds :: Link -> [(String, [(String, String)])]
getLinkIds l =
  case l of
    []                   -> []
    (q: LParam p   : rs) -> (itemString q, [(itemString q, p)]) : getLinkIds rs
    (q: LAccess ls : rs) -> (itemString q, concatMap snd $ concatMap (getLinkIds . (q : )) ls) : getLinkIds rs
    (_: rs)              -> getLinkIds rs

setLinkIds :: Link -> [String] -> String
setLinkIds _ [] = error "Error in setLinkIds, not enough parameters"
setLinkIds l (p : ps) =
  case l of
    []                    -> ""
    (_: LParam _  : rs) -> "/" ++ p ++ setLinkIds rs ps
    (_: LAccess _ : rs) -> "/" ++ p ++ setLinkIds rs ps
    (s: rs)               -> "/" ++ itemString s ++ setLinkIds rs (p: ps)
