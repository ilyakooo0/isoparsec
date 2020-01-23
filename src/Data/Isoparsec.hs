{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
    (<?>),
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
import Control.Tuple.Morph
import Data.Coerce
import Data.Isoparsec.Internal as X
import Data.Isoparsec.ToIsoparsec as X
import Data.Isoparsec.Tokenable as X
import qualified Data.Map as M
import Optics.Iso
import Optics.Optic
import Optics.Prism
import Prelude hiding ((.), fail, id)

opt :: (ArrowPlus m, IsoparsecTry m, PolyArrow m SemiIso') => m () () -> m () ()
opt m = try m <+> konst ()

opt' :: (ArrowPlus m, IsoparsecTry m, PolyArrow m SemiIso', Eq a) => a -> m () a -> m () ()
opt' a m = (try m >>> tsnok a) <+> konst ()

repeating :: (PolyArrow m SemiIso', IsoparsecTry m, ArrowPlus m, Eq b) => m () b -> m () [b]
repeating m = (try m &&& (try (repeating m) <+> konst [])) >>^ cons'

infix 0 <?>

(<?>) :: IsoparsecLabel m l => m a b -> l -> m a b
thing <?> msg = label msg thing

infixr 1 %>>

(%>>) :: (PolyArrow a SemiIso', ToSemiIso p b c) => p -> a c d -> a b d
p %>> a = si p ^>> a

infixr 1 %>%

(%>%) :: (PolyArrow a SemiIso', ToSemiIso p b c, ToSemiIso p' c d) => p -> p' -> a b d
p %>% a = si p ^>^ si a

infixr 1 >>%

(>>%) :: (PolyArrow a SemiIso', ToSemiIso p c d) => a b c -> p -> a b d
p >>% a = p >>^ si a

infixr 1 %<<

(%<<) :: (PolyArrow a SemiIso', ToSemiIso p c d) => p -> a b c -> a b d
p %<< a = si p ^<< a

infixr 1 %<%

(%<%) :: (PolyArrow a SemiIso', ToSemiIso p c d, ToSemiIso p' b c) => p -> p' -> a b d
p %<% a = si p ^<^ si a

infixr 1 <<%

(<<%) :: (PolyArrow a SemiIso', ToSemiIso p b c) => a c d -> p -> a b d
p <<% a = p <<^ si a

infixr 5 <+%

(<+%) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => a b c -> p -> a b c
a <+% b = a <+^ si b

infixr 5 %+>

(%+>) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => p -> a b c -> a b c
a %+> b = si a ^+> b

infixr 5 %+%

(%+%) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => p -> p -> a b c
a %+% b = si a ^+^ si b

infixr 0 <.>

(<.>) ::
  (PolyArrow m SemiIso', TupleMorphable x c, TupleMorphable y c, ToSemiIso b y y') =>
  b ->
  m x' x ->
  m x' y'
b <.> p = (p >>% morphed) >>% b

infixr 0 <^>

(<^>) ::
  (PolyArrow m SemiIso', TupleMorphable x c, TupleMorphable y c, ToSemiIso b y y') =>
  m x' x ->
  b ->
  m x' y'
b <^> p = b >>> morphed %>% p

coercing :: forall b a m. (Coercible a b, PolyArrow m SemiIso') => m a b
coercing = arr $ siJust coerce coerce

morphed ::
  (TupleMorphable a c, TupleMorphable b c) =>
  Iso' a b
morphed = iso morphTuples morphTuples

class ToSemiIso x a b | x -> a b where
  si :: x -> SemiIso' a b

instance (k `Is` A_Prism) => ToSemiIso (Optic' k NoIx b a) a b where
  si p = si' (Just . n) (either (const Nothing) Just . u)
    where
      (n, u) = withPrism p (,)

instance ToSemiIso (SemiIso' a b) a b where
  si = id

mapIso :: (PolyArrow m SemiIso', Ord a, Ord b) => [(a, b)] -> m a b
mapIso m = arr $ si' (`M.lookup` n) (`M.lookup` u)
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
  (Integral a, Integral b, Num a, Num b, PolyArrow m SemiIso') =>
  m a b
throughIntegral = arr $ siJust fromIntegral fromIntegral
