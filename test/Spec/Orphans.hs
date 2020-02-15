{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Orphans
  (
  )
where

import Data.ByteString
import Data.Isoparsec
import Data.Isoparsec.ByteString
import Data.Isoparsec.Chunks
import Data.Proxy
import GHC.TypeLits
import Test.Tasty.QuickCheck
import Prelude hiding ((.))

instance
  (CmpNat n 0 ~ 'GT, KnownNat n, Arbitrary (Element s), IsSequence s) =>
  Arbitrary (Chunk n s)
  where
  arbitrary = Chunk . fromList <$> vectorOf (fromIntegral $ natVal @n Proxy) arbitrary

instance Arbitrary ByteString where
  arbitrary = fromList <$> listOf arbitrary

deriving instance Arbitrary (Byte16 e)

deriving instance Arbitrary (Byte32 e)

deriving instance Arbitrary (Byte64 e)
