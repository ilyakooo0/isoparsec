{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Isoparsec
  ( module X,
    (<.>),
    repeating,
    opt,
    opt',
    morphed,
    coercing,
    mapIso,
    auto,
    specific,
    throughIntegral,
  )
where

import Control.Arrow.Extra as X
import Control.Prism
import Control.Tuple.Morph
import Data.Coerce
import Data.Either
import Data.Isoparsec.Internal as X
import Data.Isoparsec.ToIsoparsec as X
import qualified Data.Map as M
import Prelude hiding ((.), fail, id)

opt :: (ArrowPlus m, PolyArrow m SemiIso) => m () () -> m () ()
opt m = m <+^ konst ()

opt' :: (ArrowPlus m, PolyArrow m SemiIso, Eq a) => a -> m () a -> m () ()
opt' a m = opt (m >>^ turn (konst a))

repeating :: (PolyArrow m SemiIso, ArrowPlus m, Eq b) => m () b -> m () [b]
repeating m = (m &&& (repeating m <+^ konst [])) >>^ siCons

infixl 0 <.>

(<.>) ::
  (PolyArrow m SemiIso, TupleMorphable x c, TupleMorphable y c) =>
  (Market y y y (Identity y) -> Market y y y' (Identity y')) ->
  m x' x ->
  m x' y'
b <.> p = (p >>^ morphed) >>^ si b
  where
    si p' = withPrism p' $ \x y -> SemiIso (pure . x) (either (const empty) pure . y)

coercing :: forall b a. Coercible a b => SemiIso a b
coercing = siPure coerce coerce

morphed ::
  (TupleMorphable a c, TupleMorphable b c) => SemiIso a b
morphed = siPure morphTuples morphTuples

mapIso :: (PolyArrow m SemiIso, Ord a, Ord b) => [(a, b)] -> m a b
mapIso m = arr $ siMaybe (`M.lookup` n) (`M.lookup` u)
  where
    n = M.fromListWith (error "mapping not unique") m
    u =
      M.fromListWith (error "mapping not unique") $
        (\(a, b) -> (b, a)) <$> m

auto :: forall x s m. (ToIsoparsec x s, Isoparsec m s) => m () x
auto = toIsoparsec

specific :: forall x s m. (ToIsoparsec x s, Isoparsec m s, Eq x) => x -> m () ()
specific x = auto @x >>^ turn (konst x)

throughIntegral ::
  (Integral a, Integral b, Num a, Num b, PolyArrow m SemiIso) =>
  m a b
throughIntegral = arr $ siPure fromIntegral fromIntegral
