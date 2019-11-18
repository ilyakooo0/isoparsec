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
  )
where

import Control.Arrow.Extra
import Data.List.NonEmpty
import Numeric.Natural
import Optics.Iso
import Prelude hiding ((.), id)

class
  (PolyArrow m SemiIso', ArrowPlus m, ArrowChoice m, IsoparsecTry m) =>
  Isoparsec m s t
    | m -> s,
      m s -> t where

  -- {-# MINIMAL anyToken #-}

  anyToken :: m () (t)

  token :: t -> m () ()

  default tokens :: [t] -> m () ()
  tokens [] = arr $ isoConst' () ()
  tokens (t : ts) = token t &&& tokens ts >>> arr (isoConst' ((), ()) ())

  tokens :: [t] -> m () ()

  notToken :: t -> m () (t)

  default notToken :: Eq (t) => t -> m () (t)
  notToken t = tokenWhere (/= t)

  tokenWhere :: (t -> Bool) -> m () (t)
  tokenWhere f =
    anyToken >>^ si' (\t -> if f t then Just t else Nothing) Just

  manyTokens :: Natural -> m () [t]
  manyTokens 0 = arr $ isoConst' () []
  manyTokens n = anyToken &&& manyTokens (n - 1) >>^ cons'

  tokensWhile :: (t -> Bool) -> m () [t]

  default tokensWhile :: (t -> Bool) -> m () [t]
  tokensWhile f =
    try (tokenWhere f &&& tokensWhile f >>^ cons')
      <+^ isoConst' () []

  tokensWhile1 :: (t -> Bool) -> m () (NonEmpty (t))

  default tokensWhile1 :: (t -> Bool) -> m () (NonEmpty (t))
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

isoConst' :: s -> a -> SemiIso' s a
isoConst' s a = si' (const $ Just a) (const $ Just s)

konst :: PolyArrow a SemiIso' => x -> a () x
konst x = arr $ si' (const $ Just x) (const $ Just ())

tsnok :: PolyArrow a SemiIso' => x -> a x ()
tsnok x = arr $ si' (const $ Just ()) (const $ Just x)
