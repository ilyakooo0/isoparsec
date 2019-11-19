{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    consNE',
    siJust,
    siCheck,
    siCheck',
    check,
  )
where

import Control.Arrow.Extra
import Control.Monad
import Data.Isoparsec.Tokenable
import Data.List.NonEmpty
import Numeric.Natural
import Optics.Iso
import Prelude hiding ((.), id)

class
  (PolyArrow m SemiIso', ArrowPlus m, ArrowChoice m, IsoparsecTry m, Tokenable s) =>
  Isoparsec m s
    | m -> s where

  -- {-# MINIMAL anyToken #-}

  anyToken :: m () (Token s)

  token :: Token s -> m () ()

  default tokens :: [Token s] -> m () ()
  tokens [] = arr $ isoConst' () ()
  tokens (t : ts) = token t &&& tokens ts >>> arr (isoConst' ((), ()) ())

  tokens :: [Token s] -> m () ()

  notToken :: Token s -> m () (Token s)

  default notToken :: Eq (Token s) => Token s -> m () (Token s)
  notToken t = tokenWhere (/= t)

  tokenWhere :: (Token s -> Bool) -> m () (Token s)
  tokenWhere f =
    anyToken >>^ si' (\t -> if f t then Just t else Nothing) Just

  manyTokens :: Natural -> m () [Token s]
  manyTokens 0 = arr $ isoConst' () []
  manyTokens n = anyToken &&& manyTokens (n - 1) >>^ cons'

  tokensWhile :: (Token s -> Bool) -> m () [Token s]

  default tokensWhile :: (Token s -> Bool) -> m () [Token s]
  tokensWhile f =
    try (tokenWhere f &&& tokensWhile f >>^ cons')
      <+^ isoConst' () []

  tokensWhile1 :: (Token s -> Bool) -> m () (NonEmpty (Token s))

  default tokensWhile1 :: (Token s -> Bool) -> m () (NonEmpty (Token s))
  tokensWhile1 f = tokenWhere f &&& tokensWhile f >>^ consNE'

class IsoparsecLabel m l where
  label :: l -> m a b -> m a b

cons' :: SemiIso' (t, [t]) [t]
cons' =
  si'
    (Just . uncurry (:))
    ( \case
        (t : ts) -> Just (t, ts)
        _ -> Nothing
    )

consNE' :: SemiIso' (t, [t]) (NonEmpty t)
consNE' = si' (Just . uncurry (:|)) (\(t :| ts) -> Just (t, ts))

class IsoparsecTry m where
  try :: m a b -> m a b

class IsoparsecFail m e where
  fail :: e -> m a b

newtype SemiIso' s a = SemiIso' (SemiIso s s a a)

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

konst :: PolyArrow a SemiIso' => x -> a () x
konst x = arr $ si' (const $ Just x) (const $ Just ())

tsnok :: PolyArrow a SemiIso' => x -> a x ()
tsnok x = arr $ si' (const $ Just ()) (const $ Just x)

check :: PolyArrow a SemiIso' => (s -> Bool) -> a s s
check f = arr $ siCheck f id id
