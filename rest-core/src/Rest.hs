-- | These modules allow you to define a single REST resource. Then,
-- you can combine multiple resources into an API using "Rest.Api",
-- and run them using 'rest-happstack' or 'rest-snap', or generate
-- client code or documentation using 'rest-gen'.
module Rest
  ( -- | Creating a 'Resource'.
    module Rest.Resource
    -- | Defining the routing schema.
  , module Rest.Schema
    -- | Defining 'Handler's for endpoints in the resource.
  , module Rest.Handler
    -- | Combinators for defining input and ouput dictionaries of
    -- handlers.
  , module Rest.Dictionary.Combinators
    -- | Working with errors returned from handlers.
  , module Rest.Error
  ) where

import Rest.Dictionary.Combinators
import Rest.Error
import Rest.Handler ( Env (..), Handler, ListHandler, secureHandler
                    , Range (..), range, mkListing, mkOrderedListing, mkHandler
                    , mkInputHandler, mkConstHandler, mkIdHandler
                    )
import Rest.Resource (Resource, mkResource, mkResourceId, mkResourceReader, mkResourceReaderWith, Void)
import Rest.Schema
