{-# LANGUAGE GADTs, KindSignatures, TupleSections, DeriveDataTypeable #-}
module Rest.Action where

import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Monad.Error
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

data Handler m a where
  Handler :: Monad m =>
    { dictionary :: Dict id h p i o e
    , prepare    :: a -> ErrorT (Reason e) m o
    , action     :: Env id h p i -> ErrorT (Reason e) m a
    , secure     :: Bool
    } -> Handler m a

mkHandler :: Monad m => Modifier id h p i o e -> (a -> ErrorT (Reason e) m o) -> (Env id h p i -> ErrorT (Reason e) m a) -> Handler m a
mkHandler d p a = Handler (d empty) p a False

data Action m where Action :: Handler m a -> Action m

secureHandler :: Handler m a -> Handler m a
secureHandler h = h { secure = True }

secureAction :: Action m -> Action m
secureAction (Action h) = Action (secureHandler h)

mkListing
  :: Monad m
  => Modifier () () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> ((Int, Int) -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkListing d i a = mkHandler (mkPar range . d) i (a . param)

mkListingBy
  :: Monad m
  => Modifier id () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> (Env id () (Int, Int) () -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkListingBy d i a = mkHandler (mkPar range . d) i a

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
  => Modifier () () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> ((Int, Int, Maybe String, Maybe String) -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkOrderedListing d i a = mkHandler (mkPar orderedRange . d) i (a . param)

mkOrderedListingBy
  :: Monad m
  => Modifier id () () () o e
  -> ([a] -> ErrorT (Reason e) m o)
  -> (Env id () (Int, Int, Maybe String, Maybe String) () -> ErrorT (Reason e) m [a])
  -> Handler m [a]
mkOrderedListingBy d i a = mkHandler (mkPar orderedRange . d) i a

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

mkCreate :: Monad m => Modifier () () () i o e -> (a -> ErrorT (Reason e) m o) -> (i -> ErrorT (Reason e) m a) -> Action m
mkCreate d i a = Action (mkHandler d i (a . input))

mkUpdate :: Monad m => Modifier id h p i o e -> (a -> ErrorT (Reason e) m o) -> (Env id h p i -> ErrorT (Reason e) m a) -> Action m
mkUpdate d i a = Action (mkHandler d i a)

mkGetter :: Monad m => Modifier id () () () o e -> (a -> ErrorT (Reason e) m o) -> (id -> ErrorT (Reason e) m a) -> Handler m a
mkGetter d i a = mkHandler d i (a . ident)

mkGetterEnv :: Monad m => Modifier id h p i o e -> (a -> ErrorT (Reason e) m o) -> (Env id h p i -> ErrorT (Reason e) m a) -> Handler m a
mkGetterEnv d i a = mkHandler d i a

mkAction :: Monad m => Modifier () () () i o e -> (a -> ErrorT (Reason e) m o) -> (i -> ErrorT (Reason e) m a) -> Action m
mkAction d i a = Action (mkHandler d i (a . input))

mkActionEnv :: Monad m => Modifier () h p i o e -> (a -> ErrorT (Reason e) m o) -> (Env () h p i -> ErrorT (Reason e) m a) -> Action m
mkActionEnv d i a = Action (mkHandler d i a)

constHandler :: Monad m => Modifier () () () () o e -> (a -> ErrorT (Reason e) m o) -> ErrorT (Reason e) m a -> Handler m a
constHandler d i a = mkHandler d i (const a)

constAction :: Monad m => Modifier () () () () o e -> (a -> ErrorT (Reason e) m o) -> ErrorT (Reason e) m a -> Action m
constAction d i a = mkAction d i (const a)

