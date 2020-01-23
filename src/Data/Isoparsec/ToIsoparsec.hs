module Data.Isoparsec.ToIsoparsec
  ( ToIsoparsec (..),
  )
where

import Control.Arrow.Extra
import Data.Isoparsec.Internal
import Data.Isoparsec.Tokenable
import GHC.Generics
import Prelude hiding ((.))

instance {-# OVERLAPPABLE #-} t ~ Token s => ToIsoparsec t s where
  toIsoparsec = anyToken

class ToIsoparsec a s where

  toIsoparsec :: Isoparsec m s => m () a

  default toIsoparsec :: (Isoparsec m s, Generic a, GToIsoparsec (Rep a) s) => m () a
  toIsoparsec = gToIsoparsec >>^ si' (Just . to) (Just . from)

class GToIsoparsec a s where
  gToIsoparsec :: Isoparsec m s => m () (a b)

instance GToIsoparsec U1 s where
  gToIsoparsec = konst U1

instance ToIsoparsec c s => GToIsoparsec (K1 i c) s where
  gToIsoparsec =
    toIsoparsec >>^ si' (Just . K1) (Just . unK1)

instance GToIsoparsec c s => GToIsoparsec (M1 i t c) s where
  gToIsoparsec =
    gToIsoparsec >>^ si' (Just . M1) (Just . unM1)

instance (GToIsoparsec a s, GToIsoparsec b s) => GToIsoparsec (a :*: b) s where
  gToIsoparsec =
    (gToIsoparsec &&& gToIsoparsec)
      >>^ si' (\(a, b) -> Just (a :*: b)) (\(a :*: b) -> Just (a, b))

instance (GToIsoparsec a s, GToIsoparsec b s) => GToIsoparsec (a :+: b) s where
  gToIsoparsec =
    ( try (gToIsoparsec >>^ si' (Just . Left) (either Just (const Nothing)))
        <+> (gToIsoparsec >>^ si' (Just . Right) (either (const Nothing) Just))
    )
      >>> (arr (si' (Just . L1) fromL) ||| arr (si' (Just . R1) fromR))
    where
      fromL (L1 a) = Just a
      fromL _ = Nothing
      fromR (R1 b) = Just b
      fromR _ = Nothing
