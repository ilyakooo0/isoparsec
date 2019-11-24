{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Isoparsec.Internal
  ( SemiIso' (..),
    SemiIso,
    si',
    IsoparsecFail (..),
    Isoparsec (..),
    IsoparsecTry (..),
    IsoparsecLabel (..),
    konst,
    tsnok,
    cons',
    siJust,
    siCheck,
    siCheck',
    check,
    levitate,
    siDecompose,
    badKonst,
    badTsnok,
  )
where

import Control.Arrow.Extra
import Control.Monad
import Data.Isoparsec.Tokenable
import Numeric.Natural
import Optics.Iso
import Prelude as P hiding ((.), id)

class
  (PolyArrow m SemiIso', ArrowPlus m, ArrowChoice m, IsoparsecTry m, Tokenable s) =>
  Isoparsec m s
    | m -> s where

  {-# MINIMAL anyToken, manyTokens #-}

  anyToken :: m () (Token s)

  token :: Token s -> m () ()

  default token :: Eq (Token s) => Token s -> m () ()
  token x = anyToken >>> tsnok x

  tokens :: [Token s] -> m () ()

  default tokens :: [Token s] -> m () ()
  tokens [] = arr $ isoConst' () ()
  tokens (t : ts) = token t &&& tokens ts >>> arr (isoConst' ((), ()) ())

  chunk :: s -> m () ()
  chunk = tokens . lowerTokens

  notToken :: Token s -> m () (Token s)

  default notToken :: Eq (Token s) => Token s -> m () (Token s)
  notToken t = tokenWhere (/= t)

  tokenWhere :: (Token s -> Bool) -> m () (Token s)
  tokenWhere f =
    anyToken >>> check f

  manyTokens :: m Natural s

  tokensWhile :: (Token s -> Bool) -> m () s

  takeUntil :: s -> m () s

  default takeUntil :: Eq (Token s) => s -> m () s
  takeUntil s = takeUntil' s >>^ levitate
    where
      takeUntil' s' = try (chunk s' >>> konst []) <+> ((anyToken &&& takeUntil' s') >>^ cons')

  default tokensWhile :: (Token s -> Bool) -> m () s
  tokensWhile f =
    tokensWhile' f >>> check (P.all f) >>^ levitate
    where
      tokensWhile' g =
        try (tokenWhere g &&& tokensWhile' g >>^ cons')
          <+^ isoConst' () []

  tokensWhile1 :: (Token s -> Bool) -> m () s
  tokensWhile1 f =
    tokenWhere f &&& tokensWhile f
      >>^ si' (\(a, aa) -> Just $ liftToken a <> aa) levitateHead

class IsoparsecLabel m l where
  label :: l -> m a b -> m a b

levitateHead :: Tokenable s => s -> Maybe (Token s, s)
levitateHead s = case lowerTokens s of
  (t : tt) -> Just (t, liftTokens tt)
  [] -> Nothing

cons' :: SemiIso' (t, [t]) [t]
cons' =
  si'
    (Just . uncurry (:))
    ( \case
        (t : ts) -> Just (t, ts)
        _ -> Nothing
    )

levitate :: Tokenable s => SemiIso' [Token s] s
levitate = siJust liftTokens lowerTokens

class IsoparsecTry m where
  try :: m a b -> m a b

class IsoparsecFail m e where
  fail :: e -> m a b

newtype SemiIso' s a = SemiIso' (SemiIso s s a a)

siDecompose :: SemiIso' a b -> (a -> Maybe b, b -> Maybe a)
siDecompose (SemiIso' s) = withIso s (,)

type SemiIso s t a b = Iso s (Maybe t) (Maybe a) b

si' :: (s -> Maybe a) -> (a -> Maybe s) -> SemiIso' s a
si' n u = SemiIso' $ iso n u

siCheck' :: (s -> Bool) -> (s -> Maybe a) -> (a -> Maybe s) -> SemiIso' s a
siCheck' f a b =
  si'
    (\c -> guard (f c) >> a c)
    (b >=> (\c -> guard (f c) >> pure c))

siCheck :: (s -> Bool) -> (s -> a) -> (a -> s) -> SemiIso' s a
siCheck f a b = siCheck' f (Just . a) (Just . b)

siJust :: (s -> a) -> (a -> s) -> SemiIso' s a
siJust a b = si' (Just . a) (Just . b)

isoConst' :: s -> a -> SemiIso' s a
isoConst' s a = si' (const $ Just a) (const $ Just s)

konst :: (PolyArrow a SemiIso', Eq x) => x -> a () x
konst x = badKonst x >>> check (== x)

badKonst :: (PolyArrow a SemiIso') => x -> a () x
badKonst x = arr $ si' (const $ Just x) (const $ Just ())

tsnok :: (PolyArrow a SemiIso', Eq x) => x -> a x ()
tsnok x = check (== x) >>> badTsnok x

badTsnok :: (PolyArrow a SemiIso') => x -> a x ()
badTsnok x = arr $ si' (const $ Just ()) (const $ Just x)

check :: PolyArrow a SemiIso' => (s -> Bool) -> a s s
check f = arr $ siCheck f id id
