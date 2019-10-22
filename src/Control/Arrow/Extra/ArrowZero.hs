{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Extra.ArrowZero
  ( ArrowZero (..),
  )
where

import qualified Control.Arrow as A
import Control.Arrow.Extra.BaseArrow

class BaseArrow a => ArrowZero a where
  zeroArrow :: a b c

instance {-# OVERLAPPABLE #-} A.ArrowZero a => ArrowZero a where
  zeroArrow = A.zeroArrow
