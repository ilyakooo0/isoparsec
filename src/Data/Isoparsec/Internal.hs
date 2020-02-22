module Data.Isoparsec.Internal
  ( module X,
    IsoparsecFail (..),
    Isoparsec (..),
    siCons,
    unroll,
    arrowsWhile,
    arrowsUntil,
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
  (PolyArrow SemiIso m, ArrowPlus m, ArrowChoice m, IsSequence s) =>
  Isoparsec m s
    | m -> s where
  {-# MINIMAL anyToken, manyTokens, tuck #-}

  anyToken :: m () (Element s)

  token :: Element s -> m () ()
  default token :: Eq (Element s) => Element s -> m () ()
  token x = anyToken >>^ turn (konst x)

  token' :: m (Element s) (Element s)
  default token' :: Eq (Element s) => m (Element s) (Element s)
  token' = id >>& anyToken >>^ check (uncurry (==)) >>^ siPure fst (\x -> (x, x))

  tokens :: [Element s] -> m () ()
  tokens [] = arr $ isoConst () ()
  tokens (t : ts) = token t &&& tokens ts >>^ isoConst ((), ()) ()

  tokens' :: m [Element s] [Element s]
  tokens' = (turn siCons ^>> token' *** tokens' >>^ siCons) <+^ isoConst [] []

  chunk :: s -> m () ()
  chunk = tokens . otoList

  chunk' :: m s s
  chunk' = list ^>> tokens' >>^ turn list
    where
      list = siPure otoList fromList

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

arrowsWhile :: (PolyArrow SemiIso m, ArrowPlus m) => m () a -> m () [a]
arrowsWhile f = ((f &&& arrowsWhile f) >>^ siCons) <+^ isoConst () []

arrowsUntil :: (ArrowPlus m, Eq a, PolyArrow SemiIso m) => m () a -> m () b -> m () ([a], b)
arrowsUntil a b = (arr (konst []) &&& b) <+> (a &&& arrowsUntil a b >>^ (assoc >>> first siCons))

unroll :: (PolyArrow SemiIso m, ArrowPlus m, Eq a) => a -> m a (b, a) -> m () [b]
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
