-- | Combinators for specifying the input/output dictionaries of a
-- 'Handler'. The combinators can be combined using @(@'.'@)@.
module Rest.Dictionary.Combinators
  (
  -- ** Input dictionaries

    someI
  , stringI
  , xmlTextI
  , fileI
  , readI
  , xmlI
  , rawXmlI
  , jsonI

  -- ** Output dictionaries

  , someO
  , stringO
  , fileO
  , xmlO
  , rawXmlO
  , jsonO
  , multipartO

  -- ** Error dictionaries

  , someE
  , jsonE
  , xmlE

  -- ** Composed dictionaries

  , xmlJsonI
  , xmlJsonO
  , xmlJsonE
  , xmlJson

  -- ** Header dictionaries

  , mkHeader

  -- ** Parameter dictionaries

  , mkPar
  , addPar
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

someI :: Dict h p () o e -> Dict h p i o e
someI = L.set inputs (Dicts [])

-- | Allow direct usage of as input as `String`.

stringI :: Dict h p i o e -> Dict h p String o e
stringI = L.set inputs (Dicts [StringI])

-- | Allow direct usage of as input as raw Xml `Text`.

xmlTextI :: Dict h p i o e -> Dict h p Text o e
xmlTextI = L.set inputs (Dicts [XmlTextI])

-- | Allow usage of input as file contents, represented as a `ByteString`.

fileI :: Dict h p i o e -> Dict h p ByteString o e
fileI = L.set inputs (Dicts [FileI])

-- | The input can be read into some instance of `Read`. For inspection reasons
-- the type must also be an instance of both `Info` and `Show`.

readI :: (Info i, Read i, Show i) => Dict h p i o e -> Dict h p i o e
readI = L.modify (dicts . inputs) (ReadI:)

-- | The input can be read into some instance of `XmlPickler`.

xmlI :: (Typeable i, XmlPickler i) => Dict h p i o e -> Dict h p i o e
xmlI = L.modify (dicts . inputs) (XmlI:)

-- | The input can be used as an XML `ByteString`.

rawXmlI :: Dict h p i o e -> Dict h p ByteString o e
rawXmlI = L.set inputs (Dicts [RawXmlI])

-- | The input can be read into some instance of `Json`.

jsonI :: (Typeable i, FromJSON i, JSONSchema i) => Dict h p i o e -> Dict h p i o e
jsonI = L.modify (dicts . inputs) (JsonI:)

-- | Open up output type for extension with custom dictionaries.

someO :: Dict h p i () e -> Dict h p i o e
someO = L.set outputs (Dicts [])

-- | Allow output as plain String.

stringO :: Dict h p i () e -> Dict h p i String e
stringO = L.set outputs (Dicts [StringO])

-- | Allow file output using a combination of the raw data and a mime type.

fileO :: Dict h p i o e -> Dict h p i (ByteString, String) e
fileO = L.set outputs (Dicts [FileO])

-- | Allow output as XML using the `XmlPickler` type class.

xmlO :: (Typeable o, XmlPickler o) => Dict h p i o e -> Dict h p i o e
xmlO = L.modify (dicts . outputs) (XmlO:)

-- | Allow output as raw XML represented as a `ByteString`.

rawXmlO :: Dict h p i () e -> Dict h p i ByteString e
rawXmlO = L.set outputs (Dicts [RawXmlO])

-- | Allow output as JSON using the `Json` type class.

jsonO :: (Typeable o, ToJSON o, JSONSchema o) => Dict h p i o e -> Dict h p i o e
jsonO = L.modify (dicts . outputs) (JsonO:)

-- | Allow output as multipart. Writes out the ByteStrings separated
-- by boundaries, with content type 'multipart/mixed'.

multipartO :: Dict h p i () e -> Dict h p i [BodyPart] e
multipartO = L.set outputs (Dicts [MultipartO])

-- | Open up error type for extension with custom dictionaries.

someE :: (Typeable e, ToJSON e, JSONSchema e) => Dict h p i o () -> Dict h p i o e
someE = L.set errors (Dicts [])

-- | Allow error output as JSON using the `Json` type class.

jsonE :: (Typeable e, ToJSON e, JSONSchema e) => Dict h p i o e -> Dict h p i o e
jsonE = L.modify (dicts . errors) (JsonE:)

-- | Allow error output as XML using the `XmlPickler` type class.

xmlE :: (Typeable e, XmlPickler e) => Dict h p i o e -> Dict h p i o e
xmlE = L.modify (dicts . errors) (XmlE:)

-- | The input can be read into some instance of both `Json` and `XmlPickler`.

xmlJsonI :: (Typeable i, FromJSON i, JSONSchema i, XmlPickler i) => Dict h p () o e -> Dict h p i o e
xmlJsonI = xmlI . jsonI . someI

-- | Allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJsonO :: (Typeable o, ToJSON o, JSONSchema o, XmlPickler o) => Dict h p i () e -> Dict h p i o e
xmlJsonO = xmlO . jsonO . someO

-- | Allow error output as JSON using the `Json` type class and allow output as
-- XML using the `XmlPickler` type class.

xmlJsonE :: (Typeable e, ToJSON e, JSONSchema e, XmlPickler e) => Dict h p i o () -> Dict h p i o e
xmlJsonE = xmlE . jsonE . someE

-- | The input can be read into some instance of both `Json` and `XmlPickler`
-- and allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJson :: (Typeable i, FromJSON i, JSONSchema i, XmlPickler i
           ,Typeable o, ToJSON o, JSONSchema o, XmlPickler o)
        => Dict h p () () e -> Dict h p i o e
xmlJson = xmlJsonI . xmlJsonO
