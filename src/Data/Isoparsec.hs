{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Isoparsec
  ( module X,
    -- Isoparsec,
    (%>>),
    (%>%),
    (>>%),
    (%<<),
    (%<%),
    (<<%),
    (<+%),
    (%+>),
    (%+%),
    (<.>),
    (<^>),
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
import Data.Isoparsec.Tokenable as X
import qualified Data.Map as M
import Prelude hiding ((.), fail, id)

opt :: (ArrowPlus m, PolyArrow m SemiIso) => m () () -> m () ()
opt m = m <+> konst ()

opt' :: (ArrowPlus m, PolyArrow m SemiIso, Eq a) => a -> m () a -> m () ()
opt' a m = (m >>> tsnok a) <+> konst ()

repeating :: (PolyArrow m SemiIso, ArrowPlus m, Eq b) => m () b -> m () [b]
repeating m = (m &&& (repeating m <+> konst [])) >>^ siCons

infixr 1 %>>

(%>>) :: (PolyArrow a SemiIso, ToSemiIso p b c) => p -> a c d -> a b d
p %>> a = si p ^>> a

infixr 1 %>%

(%>%) :: (PolyArrow a SemiIso, ToSemiIso p b c, ToSemiIso p' c d) => p -> p' -> a b d
p %>% a = si p ^>^ si a

infixr 1 >>%

(>>%) :: (PolyArrow a SemiIso, ToSemiIso p c d) => a b c -> p -> a b d
p >>% a = p >>^ si a

infixr 1 %<<

(%<<) :: (PolyArrow a SemiIso, ToSemiIso p c d) => p -> a b c -> a b d
p %<< a = si p ^<< a

infixr 1 %<%

(%<%) :: (PolyArrow a SemiIso, ToSemiIso p c d, ToSemiIso p' b c) => p -> p' -> a b d
p %<% a = si p ^<^ si a

infixr 1 <<%

(<<%) :: (PolyArrow a SemiIso, ToSemiIso p b c) => a c d -> p -> a b d
p <<% a = p <<^ si a

infixr 5 <+%

(<+%) :: (PolyArrow a SemiIso, ArrowPlus a, ToSemiIso p b c) => a b c -> p -> a b c
a <+% b = a <+^ si b

infixr 5 %+>

(%+>) :: (PolyArrow a SemiIso, ArrowPlus a, ToSemiIso p b c) => p -> a b c -> a b c
a %+> b = si a ^+> b

infixr 5 %+%

(%+%) :: (PolyArrow a SemiIso, ArrowPlus a, ToSemiIso p b c) => p -> p -> a b c
a %+% b = si a ^+^ si b

infixr 0 <.>

(<.>) ::
  (PolyArrow m SemiIso, TupleMorphable x c, TupleMorphable y c) =>
  (Market y y y (Identity y) -> Market y y y' (Identity y')) ->
  m x' x ->
  m x' y'
b <.> p = (p >>^ morphed) >>^ si b

infixr 0 <^>

(<^>) ::
  (PolyArrow m SemiIso, TupleMorphable x c, TupleMorphable y c, ToSemiIso b y y') =>
  m x' x ->
  b ->
  m x' y'
b <^> p = b >>> morphed %>% p

coercing :: forall b a m. (Coercible a b, PolyArrow m SemiIso) => m a b
coercing = arr $ siPure coerce coerce

morphed ::
  (TupleMorphable a c, TupleMorphable b c) => SemiIso a b
morphed = siPure morphTuples morphTuples

class ToSemiIso x a b | x -> a b where
  si :: x -> SemiIso a b

instance ToSemiIso (Market a a a (Identity a) -> Market a a b (Identity b)) a b where
  si p = withPrism p $ \x y -> SemiIso (pure . x) (either (const empty) pure . y)

instance ToSemiIso (SemiIso a b) a b where
  si = id

mapIso :: (PolyArrow m SemiIso, Ord a, Ord b) => [(a, b)] -> m a b
mapIso m = arr $ siMaybe (`M.lookup` n) (`M.lookup` u)
  where
    n = M.fromListWith (error "mapping not unique") m
    u =
      M.fromListWith (error "mapping not unique") $
        (\(a, b) -> (b, a)) <$> m

auto :: forall x s m. (ToIsoparsec x s, Isoparsec m s) => m () x
auto = toIsoparsec

specific :: forall x s m. (ToIsoparsec x s, Isoparsec m s, Eq x, Show x) => x -> m () ()
specific x = auto @x >>> check (== x) >>> tsnok x

throughIntegral ::
  (Integral a, Integral b, Num a, Num b, PolyArrow m SemiIso) =>
  m a b
throughIntegral = arr $ siPure fromIntegral fromIntegral
