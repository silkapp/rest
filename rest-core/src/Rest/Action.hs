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

data Env h p i = Env
  { header :: h
  , param  :: p
  , input  :: i
  }

data GenHandler m f where
  GenHandler :: Monad m =>
    { dictionary :: Dict h p i o e
    , handler    :: Env h p i -> ErrorT (Reason e) m (Apply f o)
    , secure     :: Bool
    } -> GenHandler m f

mkHandler :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m (Apply f o)) -> GenHandler m f
mkHandler d a = GenHandler (d empty) a False

type family Apply (f :: * -> *) a :: *
type instance Apply Identity a = a
type instance Apply []       a = [a]

type Handler     m = GenHandler m Identity
type ListHandler m = GenHandler m []

secureHandler :: Handler m -> Handler m
secureHandler h = h { secure = True }

mkListing
  :: Monad m
  => Modifier () () () o e
  -> ((Int, Int) -> ErrorT (Reason e) m [o])
  -> ListHandler m
mkListing d a = mkHandler (mkPar range . d) (a . param)

mkListingBy
  :: Monad m
  => Modifier () () () o e
  -> (Env () (Int, Int) () -> ErrorT (Reason e) m [o])
  -> ListHandler m
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
  -> ListHandler m
mkOrderedListing d a = mkHandler (mkPar orderedRange . d) (a . param)

mkOrderedListingBy
  :: Monad m
  => Modifier () () () o e
  -> (Env () (Int, Int, Maybe String, Maybe String) () -> ErrorT (Reason e) m [o])
  -> ListHandler m
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

mkCreate :: Monad m => Modifier () () i o e -> (i -> ErrorT (Reason e) m o) -> Handler m
mkCreate d a = mkHandler d (a . input)

mkUpdate :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m o) -> Handler m
mkUpdate d a = mkHandler d a

mkGetter :: Monad m => Modifier () () () o e -> ErrorT (Reason e) m o -> Handler m
mkGetter d a = mkHandler d (const a)

mkGetterEnv :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m o) -> Handler m
mkGetterEnv d a = mkHandler d a

mkAction :: Monad m => Modifier () () i o e -> (i -> ErrorT (Reason e) m o) -> Handler m
mkAction d a = mkHandler d (a . input)

mkActionEnv :: Monad m => Modifier h p i o e -> (Env h p i -> ErrorT (Reason e) m o) -> Handler m
mkActionEnv d a = mkHandler d a

constHandler :: Monad m => Modifier () () () o e -> ErrorT (Reason e) m o -> Handler m
constHandler d a = mkHandler d (const a)
