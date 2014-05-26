module Rest.Gen.Base.XML
  ( getXmlSchema
  , showSchema
  , showExample
  ) where

import Data.List
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema

getXmlSchema :: XmlPickler a => proxy a -> Schema
getXmlSchema = theSchema . getPU

getPU :: XmlPickler a => proxy a -> PU a
getPU _ = xpickle

showSchema :: Schema -> String
showSchema sch =
  case result of
    []  -> ""
    [x] -> x
    xs  -> intercalate "\n" $
             ["<complexType name='data'>"]
          ++ indent xs
          ++ ["</complexType>"]
 where
  result = showSchema' "" sch

  showSchema' :: String -> Schema -> [String]
  showSchema' ats Any              = ["<xs:any" ++ ats ++ "/>"]

  showSchema' _ (Seq [])           = []
  showSchema' ats (Seq [x])        = showSchema' ats x
  showSchema' ats (Seq ss)         = ["<xs:sequence" ++ ats ++ ">"]
                                  ++ indent (concatMap (showSchema' "") ss)
                                  ++ ["</xs:sequence>"]

  showSchema' _ (Alt [])           = []
  showSchema' ats (Alt [x])        = showSchema' ats x
  showSchema' ats (Alt ss)         = ["<xs:choice" ++ ats ++ ">"]
                                  ++ indent (concatMap (showSchema' "") ss)
                                  ++ ["</xs:choice>"]

  showSchema' ats (Rep l u s)        = showSchema' (ats ++ concatMap (' ':) (mn ++ mx)) s
        where mn = if l >= 0 then ["minOccurs=" ++ show l] else []
              mx = if u >= 0 then ["maxOccurs=" ++ show u] else []

  showSchema' ats (Element n (CharData dty)) = ["<xs:element name='" ++ n ++ "' type='" ++ dataToString dty ++ "'" ++ ats ++ "/>"]
  showSchema' ats (Element n (Seq [])) = ["<xs:element name='" ++ n ++ "'" ++ ats ++ "/>"]
  showSchema' ats (Element n s)    = ["<xs:element name='" ++ n ++ "'" ++ ats ++ ">"]
                                  ++ indent (
                                         ["<xs:complexType>"]
                                      ++ indent (showSchema' "" s)
                                      ++ ["</xs:complexType>"])
                                  ++ ["</xs:element>"]

  showSchema' ats (Attribute n (CharData dty))  = ["<xs:attribute name='" ++ n ++ "' type='" ++ dataToString dty ++ "'" ++ ats ++ "/>"]
  showSchema' ats (ElemRef n)     = ["<xs:element ref='" ++ n ++ "'" ++ ats ++ "/>"]
  showSchema' _ _                 = []

  dataToString :: DataTypeDescr -> String
  dataToString (DTDescr _ n _) = "xs:" ++ n

indent :: [String] -> [String]
indent = map ("  " ++)

showExample :: Schema -> String
showExample sch = intercalate "\n" $ showExample' sch
 where
  showExample' :: Schema -> [String]
  showExample' Any              = ["<anyTag/>"]

  showExample' (Seq [])         = []
  showExample' (Seq [x])        = showExample' x
  showExample' (Seq ss)         = concatMap showExample' ss

  showExample' (Alt [])         = []
  showExample' (Alt (x : _))    = showExample' x

  showExample' (Rep _ _ s)      = showExample' s

  showExample' (Element n (CharData _)) = ["<" ++ n ++ ">string</" ++ n ++ ">"]
  showExample' (Element n (Seq [])) = ["<" ++ n ++ "/>"]
  showExample' (Element n s)    = ["<" ++ n ++ ">"]
                               ++ indent (showExample' s)
                               ++ ["</" ++ n ++ ">"]
  showExample' _                = []
