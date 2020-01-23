{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Extra.ArrowZero
  ( ArrowZero (..),
  )
where

import Control.Arrow.Extra.BaseArrow

class BaseArrow a => ArrowZero a where
  zeroArrow :: a b c
