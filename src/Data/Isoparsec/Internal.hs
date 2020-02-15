module Data.Isoparsec.Internal
  ( module X,
    IsoparsecFail (..),
    Isoparsec (..),
    konst,
    tsnok,
    siCons,
    isoCheck,
    siCheck,
    check,
    badKonst,
    badTsnok,
    unroll,
    arrowsWhile,
  )
where

import Control.Arrow.Extra
import Control.Monad
import Control.SemiIso as X hiding (filterM, replicateM)
import Data.MonoTraversable as X
import Data.Sequences as X
import Numeric.Natural
import Prelude as P hiding ((.), id)

class
  (PolyArrow m SemiIso, ArrowPlus m, ArrowChoice m, IsSequence s) =>
  Isoparsec m s
    | m -> s where
  {-# MINIMAL anyToken, manyTokens, tuck #-}

  anyToken :: m () (Element s)

  token :: Element s -> m () ()
  default token :: Eq (Element s) => Element s -> m () ()
  token x = anyToken >>^ tsnok x

  tokens :: [Element s] -> m () ()
  default tokens :: [Element s] -> m () ()
  tokens [] = arr $ isoConst () ()
  tokens (t : ts) = token t &&& tokens ts >>> arr (isoConst ((), ()) ())

  chunk :: s -> m () ()
  chunk = tokens . otoList

  notToken :: Element s -> m () (Element s)
  default notToken :: Eq (Element s) => Element s -> m () (Element s)
  notToken t = tokenWhere (/= t)

  tokenWhere :: (Element s -> Bool) -> m () (Element s)
  tokenWhere f = anyToken >>^ check f

  manyTokens :: m Natural s

  takeUntil :: s -> m () s
  default takeUntil :: Eq s => s -> m () s
  takeUntil s = (chunk s >>^ konst mempty) <+> ((anyToken &&& takeUntil s) >>^ siCons)

  tokensWhile :: (Element s -> Bool) -> m () s
  default tokensWhile :: (Element s -> Bool) -> m () s
  tokensWhile f =
    (((tokenWhere f >>^ check f) &&& tokensWhile f) >>^ siCons)
      <+^ isoConst () mempty

  tokensWhile1 :: (Element s -> Bool) -> m () s
  tokensWhile1 f = tokenWhere f &&& tokensWhile f >>^ siCons

  -- | "tucks" the context of the parser into its input.
  -- >               ┌─────┐
  -- >               │  s  ├───────┐
  -- >               └─────┘       │
  -- >    ┌────┐                   │      ┌─────┐
  -- > ───┤ () ├────▶              └──────┤  a  ├▶
  -- >    └────┘                          └─────┘
  -- >                      │
  -- >                 ┌────┴─────┐
  -- >                 │   tuck   │
  -- >                 └────┬─────┘
  -- >                      ▼
  -- >   ┌─────┐                          ┌─────┐
  -- > ──┤  s  ├──────────────────────────┤  a  ├─▶
  -- >   └─────┘                          └─────┘
  tuck :: m () a -> m s a

arrowsWhile :: (PolyArrow m SemiIso, ArrowPlus m) => m () a -> m () [a]
arrowsWhile f = ((f &&& arrowsWhile f) >>^ siCons) <+^ isoConst () []

unroll :: (PolyArrow m SemiIso, ArrowPlus m, Eq a) => a -> m a (b, a) -> m () [b]
unroll a f = (konst a ^>> unroll' f) <+^ isoConst () []
  where
    unroll' g = (g >>> second (unroll' g)) >>^ siCons

siCons :: IsSequence s => SemiIso (Element s, s) s
siCons =
  siMaybe
    (pure . uncurry cons)
    uncons

class IsoparsecFail m e where
  failure :: e -> m a b

siCheck ::
  (s -> Bool) ->
  (forall f. AlternativeMonad f => s -> f a) ->
  (forall f. AlternativeMonad f => a -> f s) ->
  SemiIso s a
siCheck f a b =
  SI
    (\c -> guard (f c) >> a c)
    (b >=> (\c -> guard (f c) >> pure c))

isoCheck :: (s -> Bool) -> (s -> a) -> (a -> s) -> SemiIso s a
isoCheck f a b = siCheck f (pure . a) (pure . b)

isoConst :: s -> a -> SemiIso s a
isoConst s a = SI (const $ pure a) (const $ pure s)

konst :: Eq x => x -> SemiIso () x
konst x = badKonst x >>> check (== x)

badKonst :: x -> SemiIso () x
badKonst x = SI (const $ pure x) (const $ pure ())

tsnok :: Eq x => x -> SemiIso x ()
tsnok x = check (== x) >>> badTsnok x

badTsnok :: x -> SemiIso x ()
badTsnok x = SI (const $ pure ()) (const $ pure x)

check :: (s -> Bool) -> SemiIso s s
check f = isoCheck f id id
