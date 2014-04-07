{-# LANGUAGE GADTs, KindSignatures, TupleSections, DeriveDataTypeable, TypeFamilies #-}
-- | Handlers for endpoints in a 'Resource'.
module Rest.Handler
  ( -- * Single handlers.
    mkHandler
  , mkInputHandler
  , mkConstHandler
  , mkIdHandler

    -- * Listings.
  , mkListing
  , mkOrderedListing
    -- ** Parameter parsers for listings.
  , Range (..)
  , range
  , orderedRange

    -- * Generic handlers and core data types.
  , Env (..)
  , GenHandler (..)
  , mkGenHandler
  , Apply
  , Handler
  , ListHandler

    -- * Convenience functions.
  , secureHandler
  ) where

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Safe

import Rest.Dictionary
import Rest.Error

-------------------------------------------------------------------------------

-- | An environment of inputs passed to a handler. Contains
-- information from the 'header's, the 'param'eters and the body
-- 'input'.

data Env h p i = Env
  { header :: h
  , param  :: p
  , input  :: i
  }

-- | A handler for some endpoint. The input and output types are
-- specified by the 'dictionary', which can be created using the
-- combinators from "Rest.Dictionary.Combinators". The inputs
-- (headers, parameters and body) are passed as an 'Env' to the
-- 'handler'. This handler runs in monad @m@, combined with the
-- ability to throw errors. The result is either the output value, or
-- a list of them for list handlers.
-- If the 'secure' flag is set, this suggests to clients that the
-- resource should only be served over https. It has no effect when
-- running the API.

data GenHandler m f where
  GenHandler ::
    { dictionary :: Dict h p i o e
    , handler    :: Env h p i -> ErrorT (Reason e) m (Apply f o)
    , secure     :: Bool
    } -> GenHandler m f

-- | Construct a 'GenHandler' using a 'Modifier' instead of a 'Dict'.
-- The 'secure' flag will be 'False'.

mkGenHandler :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m (Apply f o)) -> GenHandler m f
mkGenHandler d a = GenHandler (d empty) a False

-- | Apply a Functor @f@ to a type @a@. In general will result in @f
-- a@, except if @f@ is 'Identity', in which case it will result in
-- @a@. This prevents a lot of 'Identity' wrapping/unwrapping.

type family Apply (f :: * -> *) a :: *
type instance Apply Identity a = a
type instance Apply []       a = [a]

-- | A 'Handler' returning a single item.
type Handler     m = GenHandler m Identity
-- | A 'Handler' returning a list of items.
type ListHandler m = GenHandler m []

-- | Set 'secure' to 'True'.

secureHandler :: Handler m -> Handler m
secureHandler h = h { secure = True }

-- | Data type for representing the requested range in list handlers.

data Range = Range { offset :: Int, count :: Int }

-- | Smart constructor for creating a list handler.

mkListing
  :: Monad m
  => Modifier () () () o e
  -> (Range -> ErrorT (Reason e) m [o])
  -> ListHandler m
mkListing d a = mkGenHandler (mkPar range . d) (a . param)

-- | Dictionary for taking 'Range' parameters. Allows two query
-- parameters, @offset@ and @count@. If not passed, the defaults are 0
-- and 100. The maximum range that can be passed is 1000.

range :: Param Range
range = Param ["offset", "count"] $ \xs ->
  maybe (Left (ParseError "range"))
        (Right . normalize)
    $ case xs of
        [Just o, Just c] -> Range         <$> readMay o <*> readMay c
        [_     , Just c] -> Range 0       <$> readMay c
        [Just o, _     ] -> (`Range` 100) <$> readMay o
        _                -> Just $ Range 0 100
  where normalize r = Range { offset = max 0 . offset $ r
                            , count  = min 1000 . max 0 . count $ r
                            }

-- | Create a list handler that accepts ordering information.

mkOrderedListing
  :: Monad m
  => Modifier () () () o e
  -> ((Range, Maybe String, Maybe String) -> ErrorT (Reason e) m [o])
  -> ListHandler m
mkOrderedListing d a = mkGenHandler (mkPar orderedRange . d) (a . param)

-- | Dictionary for taking ordering information. In addition to the
-- parameters accepted by 'range', this accepts @order@ and
-- @direction@.
orderedRange :: Param (Range, Maybe String, Maybe String)
orderedRange = Param ["offset", "count", "order", "direction"] $ \xs ->
  case xs of
    [mo, mc, mor, md] ->
      maybe (Left (ParseError "range"))
            (Right . (\(o, c) -> (Range o c, mor, md)) . normalize)
        $ case (mo, mc) of
            (Just o, Just c) -> (,)    <$> readMay o <*> readMay c
            (_     , Just c) -> (0,)   <$> readMay c
            (Just o, _     ) -> (,100) <$> readMay o
            _                -> Just (0, 100)
    _ -> error "Internal error in orderedRange rest parameters"
  where normalize = (max 0 *** (min 1000 . max 0))

-- | Create a handler for a single resource. Takes the entire
-- environmend as input.

mkHandler :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m o) -> Handler m
mkHandler = mkGenHandler

-- | Create a handler for a single resource. Takes only the body
-- information as input.

mkInputHandler :: Monad m => Modifier () () i o e -> (i -> ErrorT (Reason e) m o) -> Handler m
mkInputHandler d a = mkHandler d (a . input)

-- | Create a handler for a single resource. Doesn't take any input.

mkConstHandler :: Monad m => Modifier () () () o e -> ErrorT (Reason e) m o -> Handler m
mkConstHandler d a = mkHandler d (const a)

-- | Create a handler for a single resource. Take body information and
-- the resource identifier as input. The monad @m@ should be a
-- 'Reader'-like type containing the idenfier.

mkIdHandler :: MonadReader id m => Modifier h p i o e -> (i -> id -> ErrorT (Reason e) m o) -> Handler m
mkIdHandler d a = mkHandler d (\env -> ask >>= a (input env))
