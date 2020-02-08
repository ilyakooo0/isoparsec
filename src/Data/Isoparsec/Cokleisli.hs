module Data.Isoparsec.Cokleisli
  ( Cokleisli (..),
  )
where

import Control.Applicative
import Control.Arrow.Extra
import Control.Monad
import Data.Isoparsec.Internal
import Prelude hiding ((.))

newtype Cokleisli m a b = Cokleisli {unCokleisli :: b -> m a}

instance Monad m => Category (Cokleisli m) where
  id = Cokleisli return

  (Cokleisli cb) . (Cokleisli ba) = Cokleisli $ cb >=> ba

instance (Alternative m, Monad m) => BaseArrow (Cokleisli m) where
  (Cokleisli cb) *** (Cokleisli c'b') = Cokleisli $
    \(c, c') -> do
      b' <- c'b' c'
      b <- cb c
      return (b, b')

  (Cokleisli cb) &&& (Cokleisli c'b) = Cokleisli $
    \(c, c') -> c'b c' >> cb c

instance MonadPlus m => PolyArrow (Cokleisli m) SemiIso where
  arr si = Cokleisli $ \t -> case project si t of
    Just x -> return x
    Nothing -> mzero

instance MonadPlus m => ArrowZero (Cokleisli m) where
  zeroArrow = Cokleisli $ const mzero

instance MonadPlus m => ArrowPlus (Cokleisli m) where
  (Cokleisli lhs) <+> (Cokleisli rhs) = Cokleisli $ \x -> lhs x `mplus` rhs x

instance (Alternative m, Monad m) => ArrowChoice (Cokleisli m) where
  left (Cokleisli cb) = Cokleisli $ \case
    Left c -> Left <$> cb c
    Right d -> return $ Right d

  right (Cokleisli cb) = Cokleisli $ \case
    Left d -> return $ Left d
    Right c -> Right <$> cb c

  (Cokleisli cb) +++ (Cokleisli c'b') = Cokleisli $ \case
    Left c -> Left <$> cb c
    Right c' -> Right <$> c'b' c'

  (Cokleisli db) ||| (Cokleisli dc) = Cokleisli $ \d ->
    (Left <$> db d) <|> (Right <$> dc d)
