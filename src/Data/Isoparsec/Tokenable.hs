{-# LANGUAGE TypeFamilies #-}

module Data.Isoparsec.Tokenable
  ( Tokenable (..),
  )
where

class Monoid s => Tokenable s where

  type Token s

  {-# MINIMAL (liftTokens | liftToken), lowerTokens #-}

  liftTokens :: [Token s] -> s
  liftTokens = mconcat . fmap liftToken

  liftToken :: Token s -> s
  liftToken = liftTokens . pure

  lowerTokens :: s -> [Token s]

instance Tokenable [t] where

  type Token [t] = t

  liftTokens = id

  lowerTokens = id
