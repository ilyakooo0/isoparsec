module Control.Arrow.Free
  ( FreeArrow (..),
    freeKleislir,
    freeKleislil,
    freeCokleislir,
    freeCokleislil,
    module Control.Arrow.Extra,
  )
where

import Control.Applicative
import Control.Arrow.Extra
import Control.Cokleisli
import Control.Monad
import Prelude hiding ((.))

newtype FreeArrow (m :: * -> *) (k :: (* -> *) -> * -> * -> *) (s :: * -> * -> *) a b
  = FreeArrow {runFreeArrow :: k m a b}
  deriving (Functor, Applicative, Monad, Category, BaseArrow, ArrowChoice, ArrowZero, ArrowPlus)

deriving instance (BaseArrow (k m), PolyArrow p (k m)) => PolyArrow p (FreeArrow m k)

freeKleislir :: m x -> FreeArrow m Kleisli () x
freeKleislir = FreeArrow . Kleisli . const

freeKleislil :: (Monad m, Alternative m, Eq x) => m x -> FreeArrow m Kleisli x ()
freeKleislil m = FreeArrow . Kleisli $ \x -> m >>= guard . (== x)

freeCokleislir :: (Monad m, Alternative m, Eq x) => m x -> FreeArrow m Cokleisli () x
freeCokleislir m = FreeArrow . Cokleisli $ \x -> m >>= guard . (== x)

freeCokleislil :: m x -> FreeArrow m Cokleisli x ()
freeCokleislil = FreeArrow . Cokleisli . const
-- class ArrowFree (c :: (* -> *) -> Constraint) (a :: * -> * -> *) where
--   freer :: Eq x => (forall m. c m => m x) -> a () x
--   freel :: Eq x => (forall m. c m => m x) -> a x ()
-- instance (c m, Monad m, Alternative m) => FreeArrow c (Kleisli m) where
--   freer m = Kleisli $ const m
--   freel m = Kleisli $ \x -> m >>= guard . (== x)

-- instance (Monad m, Alternative m, c m) => FreeArrow c (Cokleisli m) where
--   freer m = Cokleisli $ \x -> m >>= guard . (== x)
--   freel m = Cokleisli $ const m

-- runArrowFree :: a -> Kleisli m a b -> m b
-- runArrowFree = flip runKleisli

-- corunArrowFree :: b -> Cokleisli m a b -> m a
-- corunArrowFree = flip runCokleisli
-- bifree ::
--   forall c a x.
--   (FreeArrow c a, Category a, Eq x) =>
--   (forall m. c m => m x) ->
--   a x x ->
--   a () ()
-- bifree m a = freer @c m >>> a >>> freel @c m
