module Control.Arrow.Extra.PolyArrow
  ( PolyArrow (..),
    (^>>),
    (>>^),
    (<<^),
    (^<<),
    (^>^),
    (^<^),
  )
where

import Control.Arrow.Extra.BaseArrow

infixl 1 ^>>, >>^

infixl 1 ^<<, <<^

infixl 1 ^>^, ^<^

class BaseArrow a => PolyArrow a p where
  arr :: p b c -> a b c

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
