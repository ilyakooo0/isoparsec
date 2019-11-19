{-# LANGUAGE TypeFamilies #-}

module Data.Isoparsec.Tokenable
  ( Tokenable (..),
  )
where

class Monoid s => Tokenable s where

  type Token s

  liftToken :: Token s -> s

instance Tokenable [t] where

  type Token [t] = t

  liftToken = pure
