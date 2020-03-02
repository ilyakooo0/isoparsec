module Data.Isoparsec.Chunks
  ( Chunk (..),
  )
where

import Data.Isoparsec
import Data.Proxy
import GHC.TypeLits

newtype Chunk (n :: Nat) s = Chunk {unChunk :: s}
  deriving (Show, Eq, Ord)

instance (CmpNat n 0 ~ 'GT, KnownNat n) => ToIsoparsec (Chunk n s) s m where
  toIsoparsec =
    konst (fromIntegral $ natVal @n Proxy) ^>> manyTokens >>^ coercing
