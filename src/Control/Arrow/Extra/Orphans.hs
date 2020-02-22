{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Arrow.Extra.Orphans
  (
  )
where

import qualified Control.Arrow as A
import Control.Arrow.Extra

instance {-# OVERLAPPABLE #-} A.ArrowZero a => ArrowZero a where
  zeroArrow = A.zeroArrow

instance {-# OVERLAPPABLE #-} A.ArrowChoice a => ArrowChoice a where
  left = A.left

  right = A.right

  (+++) = (A.+++)

  (|||) = (A.|||)

instance {-# OVERLAPPABLE #-} (Category a, A.Arrow a) => BaseArrow a where
  first = A.first

  second = A.second

  (***) = (A.***)

  (&&&) = (A.&&&)

instance A.Arrow a => PolyArrow (->) a where
  arr = A.arr

instance {-# OVERLAPPABLE #-} A.ArrowPlus a => ArrowPlus a where
  (<+>) = (A.<+>)
