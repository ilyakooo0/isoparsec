{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
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
    repeating,
    repeating1,
    opt,
    morphed,
  )
where

import Control.Arrow.Extra as X
import Control.Tuple.Morph
import Data.Isoparsec.Internal as X
import Data.List.NonEmpty (NonEmpty)
import Optics.Iso
import Optics.Optic
import Optics.Prism
import Prelude hiding ((.), fail, id)

opt :: (ArrowPlus m, IsoparsecTry m, PolyArrow m SemiIso') => m () () -> m () ()
opt m = try m <+> konst ()

repeating :: (PolyArrow m SemiIso', IsoparsecTry m, ArrowPlus m) => m () b -> m () [b]
repeating m = (try m &&& (try (repeating m) <+> konst [])) >>^ cons'

repeating1 :: (PolyArrow m SemiIso', IsoparsecTry m, ArrowPlus m) => m () b -> m () (NonEmpty b)
repeating1 m = (m &&& repeating m) >>^ consNE'

infix 0 <?>

(<?>) :: IsoparsecLabel m l => m a b -> l -> m a b
thing <?> msg = label msg thing

infixl 1 %>>

(%>>) :: (PolyArrow a SemiIso', ToSemiIso p b c) => p -> a c d -> a b d
p %>> a = si p ^>> a

infixl 1 %>%

(%>%) :: (PolyArrow a SemiIso', ToSemiIso p b c, ToSemiIso p' c d) => p -> p' -> a b d
p %>% a = si p ^>^ si a

infixl 1 >>%

(>>%) :: (PolyArrow a SemiIso', ToSemiIso p c d) => a b c -> p -> a b d
p >>% a = p >>^ si a

infixl 1 %<<

(%<<) :: (PolyArrow a SemiIso', ToSemiIso p c d) => p -> a b c -> a b d
p %<< a = si p ^<< a

infixl 1 %<%

(%<%) :: (PolyArrow a SemiIso', ToSemiIso p c d, ToSemiIso p' b c) => p -> p' -> a b d
p %<% a = si p ^<^ si a

infixl 1 <<%

(<<%) :: (PolyArrow a SemiIso', ToSemiIso p b c) => a c d -> p -> a b d
p <<% a = p <<^ si a

infixl 5 <+%

(<+%) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => a b c -> p -> a b c
a <+% b = a <+^ si b

infixl 5 %+>

(%+>) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => p -> a b c -> a b c
a %+> b = si a ^+> b

infixl 5 %+%

(%+%) :: (PolyArrow a SemiIso', ArrowPlus a, ToSemiIso p b c) => p -> p -> a b c
a %+% b = si a ^+^ si b

infixr 0 <.>

(<.>) ::
  (PolyArrow m SemiIso', TupleMorphable x c, TupleMorphable y c, ToSemiIso b y y') =>
  b ->
  m x' x ->
  m x' y'
b <.> p = (p >>% morphed) >>% b

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
