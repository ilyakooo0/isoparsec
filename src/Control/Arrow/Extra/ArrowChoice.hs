{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Extra.ArrowChoice
  ( ArrowChoice (..),
  )
where

import Control.Arrow.Extra.BaseArrow
import Control.Category
import Data.Either

infixl 2 +++

infixl 2 |||

class BaseArrow a => ArrowChoice a where

  left :: a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')

  (|||) :: a b d -> a c d -> a (Either b c) d
