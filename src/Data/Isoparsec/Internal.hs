{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, LiberalTypeSynonyms, FlexibleContexts, DefaultSignatures, FunctionalDependencies, FlexibleInstances, MonoLocalBinds, LambdaCase, TypeApplications #-}

module Data.Isoparsec.Internal
  ( SemiIso'(..)
  , SemiIso
  , iso'
  , IsoparsecFail(..)
  , Isoparsec(..)
  , (<.>)
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Data.List.NonEmpty
import Numeric.Natural
import Optics.Iso

import Control.Arrow.Extra
import Control.Tuple.Morph

class (PolyArrow m SemiIso') => Isoparsec m t where
  {-# MINIMAL anyToken #-}

  anyToken :: m () t

  token :: t -> m () ()
  default token :: Eq t => t -> m () ()
  token t = tokenWhere (== t) >>^ isoConst' t ()

  notToken :: t -> m () t
  default notToken :: Eq t => t -> m () t
  notToken t = tokenWhere (/= t)

  tokenWhere :: (t -> Bool) -> m () t
  tokenWhere f =
    anyToken >>^ iso' (\t -> if f t then Just t else Nothing) Just

  tokens :: Natural -> m () [t]
  tokens 0 = arr $ isoConst' () []
  tokens n = anyToken &&& tokens (n-1) >>^ cons'

  tokensWhile :: (t -> Bool) -> m () [t]
  default tokensWhile :: (IsoparsecTry m, ArrowPlus m) => (t -> Bool) -> m () [t]
  tokensWhile f =
    try (tokenWhere f &&& tokensWhile f >>^ cons')
    <+^ isoConst' () []

  tokensWhile1 :: (t -> Bool) -> m () (NonEmpty t)
  default tokensWhile1 :: (t -> Bool) -> m () (NonEmpty t)
  tokensWhile1 f = tokenWhere f &&& tokensWhile f >>^ consNE'

cons' :: SemiIso' (t, [t]) [t]
cons' = iso' (Just . uncurry (:)) (\case
  (t:ts) -> Just (t, ts)
  _ -> Nothing)

consNE' :: SemiIso' (t, [t]) (NonEmpty t)
consNE' = iso' (Just . uncurry (:|)) (\(t :| ts) -> Just (t, ts))

class IsoparsecTry m where
  try :: m a b -> m a b

class IsoparsecFail m e where
  fail :: e -> m a b

infixr 9 <.>
(<.>)
  :: (PolyArrow m SemiIso', TupleMorphable a c, TupleMorphable b c)
  => m a' a
  -> m b b'
  -> m a' b'
a <.> b = (a >>^ semi' morphed) >>> b

morphed
  :: (TupleMorphable a c, TupleMorphable b c)
  => Iso' a b
morphed = iso morphTuples morphTuples

newtype SemiIso' s a = SemiIso' (SemiIso s s a a)
type SemiIso s t a b = Iso s (Maybe t) (Maybe a) b

iso' :: (s -> Maybe a) -> (a -> Maybe s) -> SemiIso' s a
iso' n u = SemiIso' $ iso n u

isoConst' :: s -> a -> SemiIso' s a
isoConst' s a = iso' (const $ Just a) (const $ Just s)

semi :: Iso s t a b -> SemiIso s t a b
semi i = iso (Just . f) (Just . g)
  where
    (f, g) = withIso i (,)

semi' :: Iso' s a -> SemiIso' s a
semi' = SemiIso' . semi
