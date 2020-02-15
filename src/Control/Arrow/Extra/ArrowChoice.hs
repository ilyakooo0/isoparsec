{-# LANGUAGE NoImplicitPrelude #-}

module Control.Arrow.Extra.ArrowChoice
  ( ArrowChoice (..),
  )
where

import Control.Arrow.Extra.BaseArrow
import Control.Category
import Data.Either

infixr 2 +++

infixr 2 |||

class BaseArrow a => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')

  (|||) :: a b d -> a c d -> a (Either b c) d
