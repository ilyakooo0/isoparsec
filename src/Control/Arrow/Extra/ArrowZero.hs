{-# LANGUAGE FlexibleInstances, UndecidableInstances, MonoLocalBinds #-}

module Control.Arrow.Extra.ArrowZero
  ( ArrowZero(..)
  ) where

import qualified Control.Arrow                 as A
import           Control.Arrow.Extra.BaseArrow

class BaseArrow a => ArrowZero a where
  zeroArrow :: a b c

instance A.ArrowZero a => ArrowZero a where
  zeroArrow = A.zeroArrow
