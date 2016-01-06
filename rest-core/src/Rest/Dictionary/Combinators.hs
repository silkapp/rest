{-# LANGUAGE
    DataKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  #-}
-- | Combinators for specifying the input/output dictionaries of a
-- 'Handler'. The combinators can be combined using @(@'.'@)@.
module Rest.Dictionary.Combinators
  (
  -- ** Input dictionaries

    stringI
  , xmlTextI
  , fileI
  , readI
  , xmlI
  , rawXmlI
  , jsonI

  -- ** Output dictionaries

  , stringO
  , fileO
  , xmlO
  , rawXmlO
  , jsonO
  , multipartO

  -- ** Error dictionaries

  , jsonE
  , xmlE

  -- ** Composed dictionaries

  , xmlJsonI
  , xmlJsonO
  , xmlJsonE
  , xmlJson

  -- ** Header dictionaries

  , mkHeader
  , addHeader

  -- ** Parameter dictionaries

  , mkPar
  , addPar

  -- ** Deprecated

  , someI
  , someO
  , someE
  ) where

import Prelude hiding (id, (.))

import Control.Category
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.JSON.Schema
import Data.Text.Lazy (Text)
import Data.Typeable
import Network.Multipart (BodyPart)
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Label.Total as L

import Rest.Dictionary.Types
import Rest.Info
import Rest.Types.Error

-- | Set custom sub-dictionary for recognizing headers.

mkHeader :: Header h -> Dict x p i o e -> Dict h p i o e
mkHeader = L.set headers

-- | Add custom sub-dictionary for recognizing headers.

addHeader :: Header h -> Dict h' p i o e -> Dict (h, h') p i o e
addHeader = L.modify headers . TwoHeaders

-- | Set custom sub-dictionary for recognizing parameters.

mkPar :: Param p -> Dict h x i o e -> Dict h p i o e
mkPar = L.set params

-- | Add custom sub-dictionary for recognizing parameters.

addPar :: Param p -> Dict h p' i o e -> Dict h (p, p') i o e
addPar = L.modify params . TwoParams

-- | Open up input type for extension with custom dictionaries.

{-# DEPRECATED someI "This can be safely removed, it is now just the identity." #-}
someI :: Dict h p i o e -> Dict h p i o e
someI = id

-- | Allow direct usage of as input as `String`.

stringI :: Dict h p 'Nothing o e -> Dict h p ('Just String) o e
stringI = L.set inputs (Dicts [StringI])

-- | Allow direct usage of as input as raw Xml `Text`.

xmlTextI :: Dict h p 'Nothing o e -> Dict h p ('Just Text) o e
xmlTextI = L.set inputs (Dicts [XmlTextI])

-- | Allow usage of input as file contents, represented as a `ByteString`.

fileI :: Dict h p 'Nothing o e -> Dict h p ('Just ByteString) o e
fileI = L.set inputs (Dicts [FileI])

-- | The input can be read into some instance of `Read`. For inspection reasons
-- the type must also be an instance of both `Info` and `Show`.

readI :: (Info i, Read i, Show i, FromMaybe i i' ~ i) => Dict h p i' o e -> Dict h p ('Just i) o e
readI = L.modify inputs (modDicts (ReadI:))

-- | The input can be read into some instance of `XmlPickler`.

xmlI :: (Typeable i, XmlPickler i, FromMaybe i i' ~ i) => Dict h p i' o e -> Dict h p ('Just i) o e
xmlI = L.modify inputs (modDicts (XmlI:))

-- | The input can be used as an XML `ByteString`.

rawXmlI :: Dict h p 'Nothing o e -> Dict h p ('Just ByteString) o e
rawXmlI = L.set inputs (Dicts [RawXmlI])

-- | The input can be read into some instance of `Json`.

jsonI :: (Typeable i, FromJSON i, JSONSchema i, FromMaybe i i' ~ i) => Dict h p i' o e -> Dict h p ('Just i) o e
jsonI = L.modify inputs (modDicts (JsonI:))

-- | Open up output type for extension with custom dictionaries.

{-# DEPRECATED someO "This can be safely removed, it is now just the identity." #-}
someO :: Dict h p i o e -> Dict h p i o e
someO = id

-- | Allow output as plain String.

stringO :: Dict h p i 'Nothing e -> Dict h p i ('Just String) e
stringO = L.set outputs (Dicts [StringO])

-- | Allow file output using a combination of the raw data, the file
-- name, and an attachment flag (causing the file to be downloaded by
-- browsers instead of shown). The mime type will be determined from
-- the file extension by your web server library, or
-- "application/octet-stream" with an unknown extension.

fileO :: Dict h p i 'Nothing e -> Dict h p i ('Just (ByteString, String, Bool)) e
fileO = L.set outputs (Dicts [FileO])

-- | Allow output as XML using the `XmlPickler` type class.

xmlO :: (Typeable o, XmlPickler o, FromMaybe o o' ~ o) => Dict h p i o' e -> Dict h p i ('Just o) e
xmlO = L.modify outputs (modDicts (XmlO:))

-- | Allow output as raw XML represented as a `ByteString`.

rawXmlO :: Dict h p i 'Nothing e -> Dict h p i ('Just ByteString) e
rawXmlO = L.set outputs (Dicts [RawXmlO])

-- | Allow output as JSON using the `Json` type class.

jsonO :: (Typeable o, ToJSON o, JSONSchema o, FromMaybe o o' ~ o) => Dict h p i o' e -> Dict h p i ('Just o) e
jsonO = L.modify outputs (modDicts (JsonO:))

-- | Allow output as multipart. Writes out the ByteStrings separated
-- by boundaries, with content type 'multipart/mixed'.

multipartO :: Dict h p i 'Nothing e -> Dict h p i ('Just [BodyPart]) e
multipartO = L.set outputs (Dicts [MultipartO])

-- | Open up error type for extension with custom dictionaries.

{-# DEPRECATED someE "This can be safely removed, it is now just the identity." #-}
someE :: Dict h p i o e -> Dict h p i o e
someE = id

-- | Allow error output as JSON using the `Json` type class.

jsonE :: (ToResponseCode e, Typeable e, ToJSON e, JSONSchema e, FromMaybe e e' ~ e) => Dict h p i o e' -> Dict h p i o ('Just e)
jsonE = L.modify errors (modDicts (JsonE:))

-- | Allow error output as XML using the `XmlPickler` type class.

xmlE :: (ToResponseCode e, Typeable e, XmlPickler e, FromMaybe e e' ~ e) => Dict h p i o e' -> Dict h p i o ('Just e)
xmlE = L.modify errors (modDicts (XmlE:))

-- | The input can be read into some instance of both `Json` and `XmlPickler`.

xmlJsonI :: (Typeable i, FromJSON i, JSONSchema i, XmlPickler i, FromMaybe i i' ~ i) => Dict h p i' o e -> Dict h p ('Just i) o e
xmlJsonI = xmlI . jsonI

-- | Allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJsonO :: (Typeable o, ToJSON o, JSONSchema o, XmlPickler o, FromMaybe o o' ~ o) => Dict h p i o' e -> Dict h p i ('Just o) e
xmlJsonO = xmlO . jsonO

-- | Allow error output as JSON using the `Json` type class and allow output as
-- XML using the `XmlPickler` type class.

xmlJsonE :: (ToResponseCode e, Typeable e, ToJSON e, JSONSchema e, XmlPickler e, FromMaybe e e' ~ e) => Dict h p i o e' -> Dict h p i o ('Just e)
xmlJsonE = xmlE . jsonE

-- | The input can be read into some instance of both `Json` and `XmlPickler`
-- and allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJson :: ( Typeable i, FromJSON i, JSONSchema i, XmlPickler i
           , Typeable o, ToJSON o, JSONSchema o, XmlPickler o
           , FromMaybe i i' ~ i, FromMaybe o o' ~ o
           )
        => Dict h p i' o' e -> Dict h p ('Just i) ('Just o) e
xmlJson = xmlJsonI . xmlJsonO
