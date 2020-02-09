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
    levitate,
    badKonst,
    badTsnok,
    unroll,
    arrowsWhile,
  )
where

import Control.Arrow.Extra
import Control.Monad
import Control.SemiIso as X
import Data.Isoparsec.Tokenable
import Numeric.Natural
import Prelude as P hiding ((.), id)

class
  (PolyArrow m SemiIso, ArrowPlus m, ArrowChoice m, Tokenable s) =>
  Isoparsec m s
    | m -> s where
  {-# MINIMAL anyToken, manyTokens, tuck #-}

  anyToken :: m () (Token s)

  token :: Token s -> m () ()
  default token :: Eq (Token s) => Token s -> m () ()
  token x = anyToken >>> tsnok x

  tokens :: [Token s] -> m () ()
  default tokens :: [Token s] -> m () ()
  tokens [] = arr $ isoConst () ()
  tokens (t : ts) = token t &&& tokens ts >>> arr (isoConst ((), ()) ())

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
      takeUntil' s' = (chunk s' >>> konst []) <+> ((anyToken &&& takeUntil' s') >>^ siCons)

  default tokensWhile :: (Token s -> Bool) -> m () s
  tokensWhile f =
    tokensWhile' f >>> check (P.all f) >>^ levitate
    where
      tokensWhile' g =
        (tokenWhere g &&& tokensWhile' g >>^ siCons)
          <+^ isoConst () []

  tokensWhile1 :: (Token s -> Bool) -> m () s
  tokensWhile1 f =
    tokenWhere f &&& tokensWhile f
      >>^ SI (\(a, aa) -> pure $ liftToken a <> aa) levitateHead

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
unroll a f = (konst a >>> unroll' f) <+^ isoConst () []
  where
    unroll' g = (g >>> second (unroll' g)) >>^ siCons

levitateHead :: Alternative f => Tokenable s => s -> f (Token s, s)
levitateHead s = case lowerTokens s of
  (t : tt) -> pure (t, liftTokens tt)
  [] -> empty

siCons :: SemiIso (t, [t]) [t]
siCons =
  SI
    (pure . uncurry (:))
    ( \case
        (t : ts) -> pure (t, ts)
        _ -> empty
    )

levitate :: Tokenable s => SemiIso [Token s] s
levitate = siPure liftTokens lowerTokens

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

konst :: (PolyArrow a SemiIso, Eq x) => x -> a () x
konst x = badKonst x ^>> check (== x)

badKonst :: x -> SemiIso () x
badKonst x = SI (const $ pure x) (const $ pure ())

tsnok :: (PolyArrow a SemiIso, Eq x) => x -> a x ()
tsnok x = check (== x) >>> badTsnok x

badTsnok :: (PolyArrow a SemiIso) => x -> a x ()
badTsnok x = arr $ SI (const $ pure ()) (const $ pure x)

check :: PolyArrow a SemiIso => (s -> Bool) -> a s s
check f = arr $ isoCheck f id id
