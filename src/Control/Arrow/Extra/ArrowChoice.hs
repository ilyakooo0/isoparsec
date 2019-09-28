{-# LANGUAGE FlexibleInstances, UndecidableInstances, MonoLocalBinds, NoImplicitPrelude #-}

module Control.Arrow.Extra.ArrowChoice
  ( ArrowChoice(..)
  ) where

import qualified Control.Arrow                 as A
import           Control.Arrow.Extra.BaseArrow
import           Control.Category
import           Data.Either


class BaseArrow a => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')

  (|||) :: a b d -> a c d -> a (Either b c) d

instance A.ArrowChoice a => ArrowChoice a where
  left = A.left
  right = A.right
  (+++) = (A.+++)
  (|||) = (A.|||)
