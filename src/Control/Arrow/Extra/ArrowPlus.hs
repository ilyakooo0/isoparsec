module Control.Arrow.Extra.ArrowPlus
  ( ArrowPlus (..),
  )
where

import Control.Arrow.Extra.ArrowZero

infixl 5 <+>

class ArrowZero a => ArrowPlus a where
  (<+>) :: a b c -> a b c -> a b c
