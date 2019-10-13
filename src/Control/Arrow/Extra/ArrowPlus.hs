{-# LANGUAGE FlexibleInstances, UndecidableInstances, MonoLocalBinds #-}

module Control.Arrow.Extra.ArrowPlus
  ( ArrowPlus(..)
  ) where

import qualified Control.Arrow                 as A
import           Control.Arrow.Extra.ArrowZero

infixr 5 <+>

class ArrowZero a => ArrowPlus a where
  (<+>) :: a b c -> a b c -> a b c

instance A.ArrowPlus a => ArrowPlus a where
  (<+>) = (A.<+>)
