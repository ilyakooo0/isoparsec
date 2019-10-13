{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MonoLocalBinds #-}

module Control.Arrow.Extra.PolyArrow
  ( PolyArrow(..)
  , (^>>)
  , (>>^)
  , (<<^)
  , (^<<)
  , (^>^)
  , (^<^)
  ) where

import qualified Control.Arrow                 as A
import           Control.Arrow.Extra.BaseArrow
import           Control.Category

infixr 1 ^>>, >>^
infixr 1 ^<<, <<^
infixr 1 ^>^, ^<^

class BaseArrow a => PolyArrow a p where
  arr :: p b c -> a b c

instance A.Arrow a => PolyArrow a (->) where
  arr = A.arr

(^>>) :: PolyArrow a p => p b c -> a c d -> a b d
f ^>> a = arr f >>> a

(>>^) :: PolyArrow a p => a b c -> p c d -> a b d
a >>^ f = a >>> arr f

(<<^) :: PolyArrow a p => a c d -> p b c -> a b d
a <<^ f = a <<< arr f

(^<<) :: PolyArrow a p => p c d -> a b c -> a b d
f ^<< a = arr f <<< a

(^>^) :: PolyArrow a p => p b c -> p c d -> a b d
a ^>^ b = arr a >>> arr b

(^<^) :: PolyArrow a p => p c d -> p b c -> a b d
a ^<^ b = arr a <<< arr b
