{-|
A dictionary (`Dict`) describes how to convert Haskell values to and from a web
representation. Rest resources internally use plain Haskell datatypes, while
communication to the outside world mostly happens using XML, JSON, plain text,
query parameters, etc. The `Dict` datatype describes how to convert resource
/indentifiers, input, request parameters, request headers, output, and errors/
to and from a Haskell representation.

The `Dict` datatype and most functions working on it take a type parameters for
every aspect of its communication, which can grow quickly. This module and
most code that depend on it uses the implicit convention of using the type
variable `id` for the resource identifier, the `h` for the request headers, the
`p` for the request parameters, the `i` for the request body, the `o` for the
response body, and the `e` for a possible error.
-}

module Rest.Dictionary
  ( module Rest.Dictionary.Types
  , module Rest.Dictionary.Combinators
  ) where

import Rest.Dictionary.Types
import Rest.Dictionary.Combinators
