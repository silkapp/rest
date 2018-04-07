module Rest.Types.Range (Range (..)) where

import Prelude.Compat

-- | Data type for representing the requested range in list handlers.

data Range = Range { offset :: Int, count :: Int }
