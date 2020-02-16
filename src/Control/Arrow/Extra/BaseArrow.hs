{-# LANGUAGE NoImplicitPrelude #-}

module Control.Arrow.Extra.BaseArrow
  ( BaseArrow (..),
    (>>>),
    (<<<),
  )
where

import Control.Category hiding ((<<<), (>>>))

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

infixl 1 >>>

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
x >>> y = y . x

infixl 1 <<<

(<<<) :: Category cat => cat b c -> cat a b -> cat a c
x <<< y = x . y
