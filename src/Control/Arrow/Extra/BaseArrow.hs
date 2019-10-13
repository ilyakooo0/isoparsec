{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module Control.Arrow.Extra.BaseArrow
  ( BaseArrow(..)
  ) where

import qualified Control.Arrow    as A
import           Control.Category

infixr 3 ***
infixr 3 &&&

class Category a => BaseArrow a where
  {-# MINIMAL (***), (&&&) #-}

  first :: a b c -> a (b,d) (c,d)
  first = (*** id)

  second :: a b c -> a (d,b) (d,c)
  second = (id ***)

  (***) :: a b c -> a b' c' -> a (b,b') (c,c')

  (&&&) :: a b c -> a b c' -> a b (c,c')

instance (Category a, A.Arrow a) => BaseArrow a where
  first = A.first
  second = A.second
  (***) = (A.***)
  (&&&) = (A.&&&)
