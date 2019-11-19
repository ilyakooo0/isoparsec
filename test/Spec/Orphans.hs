{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Orphans
  (
  )
where

import Data.Isoparsec
import Data.Isoparsec.Chunks
import Data.Proxy
import GHC.TypeLits
import Test.Tasty.QuickCheck
import Prelude hiding ((.))

instance
  (CmpNat n 0 ~ 'GT, KnownNat n, Arbitrary (Token s), Tokenable s) =>
  Arbitrary (Chunk n s)
  where
  arbitrary = Chunk . liftTokens <$> vectorOf (fromIntegral $ natVal @n Proxy) arbitrary
