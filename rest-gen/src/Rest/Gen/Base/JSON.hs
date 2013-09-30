module Rest.Gen.Base.JSON where

import Data.JSON.Schema
import Rest.Gen.Base.JSON.Pretty
import Text.JSON
import Text.PrettyPrint.HughesPJ

showExample :: Schema -> String
showExample = render . pp_value . showExample'
  where
    showExample' (Choice [])     = JSNull -- Cannot create zero value
    showExample' (Choice (x:_))  = showExample' x
    showExample' (Object fs)     = JSObject $ toJSObject $ map (\f -> (key f, showExample' (content f))) fs
    showExample' (Tuple vs)      = JSArray $ map showExample' vs
    showExample' (Array l _ _ v) = JSArray $ replicate (l `max` 1) (showExample' v)
    showExample' (Value _ _)     = JSString (toJSString "value")
    showExample' Boolean         = JSBool True
    showExample' (Number l _)    = JSRational False (toRational l)
    showExample' Null            = JSNull

