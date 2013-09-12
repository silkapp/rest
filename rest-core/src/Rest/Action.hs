{-# LANGUAGE GADTs, KindSignatures, TupleSections, DeriveDataTypeable, TypeFamilies #-}
module Rest.Action where

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad.Error
import Control.Monad.Identity
import Safe

import Rest.Dictionary
import Rest.Error

-------------------------------------------------------------------------------

-- todo: update doc.
--
-- Actions take an input of some format to an output of some format, possible
-- producing some error. Because we don't really care about the types for the
-- input, output and error types on the outside, we use an existential type to
-- hide them. We use generic input/output dictionaries to allow conversions
-- from/to different IO types. Only the context the actions runs in `m' is
-- visible.

data Env id h p i = Env
  { ident  :: id
  , header :: h
  , param  :: p
  , input  :: i
  }

data GenHandler id m f where
  GenHandler :: Monad m =>
    { dictionary :: Dict h p i o e
    , action     :: Env id h p i -> ErrorT (Reason e) m (Apply f o)
    , secure     :: Bool
    } -> GenHandler id m f

mkHandler :: Monad m => Modifier h p i o e -> (Env id h p i -> ErrorT (Reason e) m (Apply f o)) -> GenHandler id m f
mkHandler d a = GenHandler (d empty) a False

type family Apply (f :: * -> *) a :: *
type instance Apply Identity a = a
type instance Apply []       a = [a]

type Handler     id m = GenHandler id m Identity
type ListHandler id m = GenHandler id m []

secureHandler :: Handler m a -> Handler m a
secureHandler h = h { secure = True }

mkListing
  :: Monad m
  => Modifier () () () o e
  -> ((Int, Int) -> ErrorT (Reason e) m [o])
  -> ListHandler id m
mkListing d a = mkHandler (mkPar range . d) (a . param)

mkListingBy
  :: Monad m
  => Modifier () () () o e
  -> (Env id () (Int, Int) () -> ErrorT (Reason e) m [o])
  -> ListHandler id m
mkListingBy d a = mkHandler (mkPar range . d) a

range :: Param (Int, Int)
range = Param ["offset", "count"] $ \xs ->
  maybe (Left (ParseError "range"))
        (Right . normalize)
    $ case xs of
        [Just o, Just c] -> (,)    <$> readMay o <*> readMay c
        [_     , Just c] -> (0,)   <$> readMay c
        [Just o, _     ] -> (,100) <$> readMay o
        _                -> Just (0, 100)
  where normalize = (max 0 *** (min 1000 . max 0))

mkOrderedListing
  :: Monad m
  => Modifier () () () o e
  -> ((Int, Int, Maybe String, Maybe String) -> ErrorT (Reason e) m [o])
  -> ListHandler id m
mkOrderedListing d a = mkHandler (mkPar orderedRange . d) (a . param)

mkOrderedListingBy
  :: Monad m
  => Modifier () () () o e
  -> (Env id () (Int, Int, Maybe String, Maybe String) () -> ErrorT (Reason e) m [o])
  -> ListHandler id m
mkOrderedListingBy d a = mkHandler (mkPar orderedRange . d) a

orderedRange :: Param (Int, Int, Maybe String, Maybe String)
orderedRange = Param ["offset", "count", "order", "direction"] $ \xs ->
  case xs of
    [mo, mc, mor, md] ->
      maybe (Left (ParseError "range"))
            (Right . (\(o, c) -> (o, c, mor, md)) . normalize)
        $ case (mo, mc) of
            (Just o, Just c) -> (,)    <$> readMay o <*> readMay c
            (_     , Just c) -> (0,)   <$> readMay c
            (Just o, _     ) -> (,100) <$> readMay o
            _                -> Just (0, 100)
    _ -> error "Internal error in orderedRange rest parameters"
  where normalize = (max 0 *** (min 1000 . max 0))

mkCreate :: Monad m => Modifier () () i o e -> (i -> ErrorT (Reason e) m o) -> Handler () m
mkCreate d a = mkHandler d (a . input)

mkUpdate :: Monad m => Modifier h p i o e -> (Env id h p i -> ErrorT (Reason e) m o) -> Handler id m
mkUpdate d a = mkHandler d a

mkGetter :: Monad m => Modifier () () () o e -> (id -> ErrorT (Reason e) m o) -> Handler id m
mkGetter d a = mkHandler d (a . ident)

mkGetterEnv :: Monad m => Modifier h p i o e -> (Env id h p i -> ErrorT (Reason e) m o) -> Handler id m
mkGetterEnv d a = mkHandler d a

mkAction :: Monad m => Modifier () () i o e -> (i -> ErrorT (Reason e) m o) -> Handler id m
mkAction d a = mkHandler d (a . input)

mkActionEnv :: Monad m => Modifier h p i o e -> (Env id h p i -> ErrorT (Reason e) m o) -> Handler id m
mkActionEnv d a = mkHandler d a

constHandler :: Monad m => Modifier () () () o e -> ErrorT (Reason e) m o -> Handler id m
constHandler d a = mkHandler d (const a)
