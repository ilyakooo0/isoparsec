{-# LANGUAGE MonoLocalBinds #-}

module Control.Arrow.Extra
  ( module X,
    (<+^),
    (^+>),
    (^+^),
  )
where

import Control.Arrow as X (Kleisli (..))
import Control.Arrow.Extra.ArrowChoice as X
import Control.Arrow.Extra.ArrowPlus as X
import Control.Arrow.Extra.ArrowZero as X
import Control.Arrow.Extra.BaseArrow as X
import Control.Arrow.Extra.PolyArrow as X
import Control.Category as X hiding ((<<<), (>>>))

infixl 5 <+^

infixl 5 ^+>

infixl 5 ^+^

(<+^) :: (ArrowPlus a, PolyArrow p a) => a b c -> p b c -> a b c
a <+^ b = a <+> arr b

(^+>) :: (ArrowPlus a, PolyArrow p a) => p b c -> a b c -> a b c
a ^+> b = arr a <+> b

(^+^) :: (ArrowPlus a, PolyArrow p a) => p b c -> p b c -> a b c
a ^+^ b = arr a <+> arr b
