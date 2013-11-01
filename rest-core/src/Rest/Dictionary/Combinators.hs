module Rest.Dictionary.Combinators
  (
  -- ** Header dictionaries

    mkHeader

  -- ** Parameter dictionaries

  , mkPar
  , addPar

  -- ** Input dictionaries

  , someI
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

  -- ** Error dictionaries

  , someE
  , jsonE
  , xmlE

  -- ** Composed dictionaries

  , xmlJsonI
  , xmlJsonO
  , xmlJsonE
  , xmlJson
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.JSON.Schema
import Data.Text.Lazy (Text)
import Data.Typeable
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Label.Total as L

import Rest.Dictionary.Types
import Rest.Info

-- | Add custom sub-dictionary for recognizing headers.

mkHeader :: Header h -> Dict x p i o e -> Dict h p i o e
mkHeader = L.set headers

-- | Set custom sub-dictionary for recognizing parameters.

mkPar :: Param p -> Dict h x i o e -> Dict h p i o e
mkPar = L.set params

-- | Add custom sub-dictionary for recognizing parameters.

addPar :: Param p -> Dict h p' i o e -> Dict h (p, p') i o e
addPar = L.modify params . TwoParams

-- | Open up input type for extension with custom dictionaries.

someI :: Dict h p () o e -> Dict h p i o e
someI = L.set inputs []

-- | Allow direct usage of as input as `String`.

stringI :: Dict h p i o e -> Dict h p String o e
stringI = L.set inputs [StringI]

-- | Allow direct usage of as input as raw Xml `Text`.

xmlTextI :: Dict h p i o e -> Dict h p Text o e
xmlTextI = L.set inputs [XmlTextI]

-- | Allow usage of input as file contents, represented as a `ByteString`.

fileI :: Dict h p i o e -> Dict h p ByteString o e
fileI = L.set inputs [FileI]

-- | The input can be read into some instance of `Read`. For inspection reasons
-- the type must also be an instance of both `Info` and `Show`.

readI :: (Info i, Read i, Show i) => Dict h p i o e -> Dict h p i o e
readI = L.modify inputs (ReadI:)

-- | The input can be read into some instance of `XmlPickler`.

xmlI :: (Typeable i, XmlPickler i) => Dict h p i o e -> Dict h p i o e
xmlI = L.modify inputs (XmlI:)

-- | The input can be used as an XML `ByteString`.

rawXmlI :: Dict h p i o e -> Dict h p ByteString o e
rawXmlI = L.set inputs [RawXmlI]

-- | The input can be read into some instance of `Json`.

jsonI :: (Typeable i, Json i) => Dict h p i o e -> Dict h p i o e
jsonI = L.modify inputs (JsonI:)

-- | Open up output type for extension with custom dictionaries.

someO :: Dict h p i () e -> Dict h p i o e
someO = L.set outputs []

-- | Allow output as plain String.

stringO :: Dict h p i () e -> Dict h p i String e
stringO = L.set outputs [StringO]

-- | Allow file output using a combination of the raw data and a mime type.

fileO :: Dict h p i o e -> Dict h p i (ByteString, String) e
fileO = L.set outputs [FileO]

-- | Allow output as XML using the `XmlPickler` type class.

xmlO :: (Typeable o, XmlPickler o) => Dict h p i o e -> Dict h p i o e
xmlO = L.modify outputs (XmlO:)

-- | Allow output as raw XML represented as a `ByteString`.

rawXmlO :: Dict h p i () e -> Dict h p i ByteString e
rawXmlO = L.set outputs [RawXmlO]

-- | Allow output as JSON using the `Json` type class.

jsonO :: (Typeable o, Json o) => Dict h p i o e -> Dict h p i o e
jsonO = L.modify outputs (JsonO:)

-- | Open up error type for extension with custom dictionaries.

someE :: (Typeable e, Json e) => Dict h p i o () -> Dict h p i o e
someE = L.set errors []

-- | Allow error output as JSON using the `Json` type class.

jsonE :: (Typeable e, Json e) => Dict h p i o e -> Dict h p i o e
jsonE = L.modify errors (JsonE:)

-- | Allow error output as XML using the `XmlPickler` type class.

xmlE :: (Typeable e, XmlPickler e) => Dict h p i o e -> Dict h p i o e
xmlE = L.modify errors (XmlE:)

-- | The input can be read into some instance of both `Json` and `XmlPickler`.

xmlJsonI :: (Typeable i, Json i, XmlPickler i) => Dict h p () o e -> Dict h p i o e
xmlJsonI = xmlI . jsonI . someI

-- | Allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJsonO :: (Typeable o, Json o, XmlPickler o) => Dict h p i () e -> Dict h p i o e
xmlJsonO = xmlO . jsonO . someO

-- | Allow error output as JSON using the `Json` type class and allow output as
-- XML using the `XmlPickler` type class.

xmlJsonE :: (Typeable e, Json e, XmlPickler e) => Dict h p i o () -> Dict h p i o e
xmlJsonE = xmlE . jsonE . someE

-- | The input can be read into some instance of both `Json` and `XmlPickler`
-- and allow output as JSON using the `Json` type class and allow output as XML
-- using the `XmlPickler` type class.

xmlJson :: (Typeable i, Typeable o, Json i, Json o, XmlPickler i, XmlPickler o) => Dict h p () () e -> Dict h p i o e
xmlJson = xmlJsonI . xmlJsonO
