-- | Data types for representing and showing multi-part bodies.
-- Copied from a hidden module in the cgi package.
module Rest.Multipart
  ( MultiPart (..)
  , BodyPart (..)
  , HeaderName (..)
  , showMultipartBody
  ) where

import Data.Char (toLower)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intersperse)

import qualified Data.ByteString.Lazy.Char8 as BS

showMultipartBody :: String -> MultiPart -> ByteString
showMultipartBody b (MultiPart bs) =
    unlinesCRLF $ foldr (\x xs -> d:showBodyPart x:xs) [c,BS.empty] bs
 where d = BS.pack ("--" ++ b)
       c = BS.pack ("--" ++ b ++ "--")

showBodyPart :: BodyPart -> ByteString
showBodyPart (BodyPart hs c) =
    unlinesCRLF $ [BS.pack (n++": "++v) | (HeaderName n,v) <- hs] ++ [BS.empty,c]


data MultiPart = MultiPart [BodyPart]
               deriving (Show, Eq, Ord)

data BodyPart = BodyPart Headers ByteString
                deriving (Show, Eq, Ord)

-- | HTTP headers.
type Headers = [(HeaderName, String)]

-- | A string with case insensitive equality and comparisons.
newtype HeaderName = HeaderName String deriving (Show)

instance Eq HeaderName where
    HeaderName x == HeaderName y = map toLower x == map toLower y

instance Ord HeaderName where
    HeaderName x `compare` HeaderName y = map toLower x `compare` map toLower y

--
-- * RFC 2046 CRLF
--

crlf :: ByteString
crlf = BS.pack "\r\n"

unlinesCRLF :: [ByteString] -> ByteString
unlinesCRLF = BS.concat . intersperse crlf
