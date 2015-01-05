module Rest.Types.Method (Method (..)) where


data Method = GET | PUT | POST | DELETE
  deriving (Show, Eq)
