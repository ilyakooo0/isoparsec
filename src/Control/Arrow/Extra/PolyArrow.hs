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

class BaseArrow a => PolyArrow p a where
  arr :: p b c -> a b c

(^>>) :: PolyArrow p a => p b c -> a c d -> a b d
f ^>> a = arr f >>> a

(>>^) :: PolyArrow p a => a b c -> p c d -> a b d
a >>^ f = a >>> arr f

(<<^) :: PolyArrow p a => a c d -> p b c -> a b d
a <<^ f = a <<< arr f

(^<<) :: PolyArrow p a => p c d -> a b c -> a b d
f ^<< a = arr f <<< a

(^>^) :: PolyArrow p a => p b c -> p c d -> a b d
a ^>^ b = arr a >>> arr b

(^<^) :: PolyArrow p a => p c d -> p b c -> a b d
a ^<^ b = arr a <<< arr b
