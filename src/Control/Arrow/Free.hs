module Control.Arrow.Free
  ( FreeArrow (..),
    runArrowFree,
    corunArrowFree,
    module Control.Arrow.Extra,
    bifree,
  )
where

import Control.Applicative
import Control.Arrow.Extra
import Control.Cokleisli
import Control.Monad
import Data.Kind
import Prelude hiding ((.))

class FreeArrow (c :: (* -> *) -> Constraint) (a :: * -> * -> *) where
  freer :: Eq x => (forall m. c m => m x) -> a () x
  freel :: Eq x => (forall m. c m => m x) -> a x ()

instance (c m, Monad m, Alternative m) => FreeArrow c (Kleisli m) where
  freer m = Kleisli $ const m
  freel m = Kleisli $ \x -> m >>= guard . (== x)

instance (Monad m, Alternative m, c m) => FreeArrow c (Cokleisli m) where
  freer m = Cokleisli $ \x -> m >>= guard . (== x)
  freel m = Cokleisli $ const m

runArrowFree :: a -> Kleisli m a b -> m b
runArrowFree = flip runKleisli

corunArrowFree :: b -> Cokleisli m a b -> m a
corunArrowFree = flip runCokleisli

bifree ::
  forall c a x.
  (FreeArrow c a, Category a, Eq x) =>
  (forall m. c m => m x) ->
  a x x ->
  a () ()
bifree m a = freer @c m >>> a >>> freel @c m
