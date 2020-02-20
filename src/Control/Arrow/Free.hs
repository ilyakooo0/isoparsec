module Control.Arrow.Free
  ( FreeArrow (..),
    runArrowFree,
    corunArrowFree,
    module Control.Arrow.Extra,
  )
where

import Control.Applicative
import Control.Arrow.Extra
import Control.Cokleisli
-- import Control.Monad.Reader
import Control.Monad
import Prelude hiding ((.))

-- class ArrowReader (a :: * -> * -> *) r | a -> r where
--   askr :: a () r
--   askl :: a r ()

-- instance (MonadReader r m, Eq r, Alternative m) => ArrowReader (Kleisli m) r where
--   askr = Kleisli $ const ask
--   askl = Kleisli $ \r -> ask >>= guard . (== r)

-- instance (MonadReader r m, Eq r, Alternative m) => ArrowReader (Cokleisli m) r where
--   askr = Cokleisli $ \r -> ask >>= guard . (== r)
--   askl = Cokleisli $ const ask

class FreeArrow (m :: * -> *) (a :: * -> * -> *) where
  freer :: Eq x => m x -> a () x
  freel :: Eq x => m x -> a x ()

instance (Monad m, Alternative m) => FreeArrow m (Kleisli m) where
  freer = Kleisli . const
  freel m = Kleisli $ \x -> m >>= guard . (== x)

instance (Monad m, Alternative m) => FreeArrow m (Cokleisli m) where
  freer m = Cokleisli $ \x -> m >>= guard . (== x)
  freel = Cokleisli . const

runArrowFree :: Kleisli m a b -> a -> m b
runArrowFree = runKleisli

corunArrowFree :: Cokleisli m a b -> b -> m a
corunArrowFree = runCokleisli
