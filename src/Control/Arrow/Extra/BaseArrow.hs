{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Arrow.Extra.BaseArrow
  ( BaseArrow (..),
  )
where

import Control.Category

infixl 3 ***

infixl 3 &&&

class Category a => BaseArrow a where

  {-# MINIMAL (***), (&&&) #-}

  first :: a b c -> a (b, d) (c, d)
  first = (*** id)

  second :: a b c -> a (d, b) (d, c)
  second = (id ***)

  (***) :: a b c -> a b' c' -> a (b, b') (c, c')

  (&&&) :: a b c -> a b c' -> a b (c, c')
