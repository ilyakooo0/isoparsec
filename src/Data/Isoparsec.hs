{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Isoparsec
  ( module X,
    Isoparsec,
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
  )
where

import Control.Arrow.Extra as X
import Control.Tuple.Morph
import Data.Isoparsec.Internal as X
import Optics.Iso
import Optics.Optic
import Optics.Prism
import Prelude hiding ((.), fail, id)

-- | 'm' -- the arrow
-- | 'ats' -- atomic tokens like 'Char'
-- | 'ts' -- the types of tokens the parser should accept in `token`
type family Isoparsec m (ats :: [*]) (ts :: [*]) where
  Isoparsec m '[] '[] =
    ( IsoparsecTry m,
      ArrowPlus m,
      ArrowChoice m,
      PolyArrow m SemiIso'
    )
  Isoparsec m '[] (t ': ts) =
    ( IsoparsecTokenable t m,
      Isoparsec m '[] ts
    )
  Isoparsec m (at ': ats) ts =
    ( IsoparsecBase at m,
      IsoparsecTokenable at m,
      Isoparsec m ats ts
    )

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

infixl 6 <?>

(<?>) :: (IsoparsecFail m e, ArrowPlus m) => m a b -> e -> m a b
m <?> e = m <+> fail e

infixr 0 <.>

(<.>) ::
  (PolyArrow m SemiIso', TupleMorphable x c, TupleMorphable y c, ToSemiIso b y y') =>
  b ->
  m x' x ->
  m x' y'
b <.> p = (p >>% morphed) >>^ si b

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
