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
  toIsoparsec = gToIsoparsec >>^ SI (pure . to) (pure . from)

class GToIsoparsec a s where
  gToIsoparsec :: Isoparsec m s => m () (a b)

instance GToIsoparsec U1 s where
  gToIsoparsec = konst U1

instance ToIsoparsec c s => GToIsoparsec (K1 i c) s where
  gToIsoparsec =
    toIsoparsec >>^ SI (pure . K1) (pure . unK1)

instance GToIsoparsec c s => GToIsoparsec (M1 i t c) s where
  gToIsoparsec =
    gToIsoparsec >>^ SI (pure . M1) (pure . unM1)

instance (GToIsoparsec a s, GToIsoparsec b s) => GToIsoparsec (a :*: b) s where
  gToIsoparsec =
    (gToIsoparsec &&& gToIsoparsec)
      >>^ SI (\(a, b) -> pure (a :*: b)) (\(a :*: b) -> pure (a, b))

instance (GToIsoparsec a s, GToIsoparsec b s) => GToIsoparsec (a :+: b) s where
  gToIsoparsec =
    ( (gToIsoparsec >>^ SI (pure . Left) (either pure (const empty)))
        <+> (gToIsoparsec >>^ SI (pure . Right) (either (const empty) pure))
    )
      >>> (arr (SI (pure . L1) fromL) ||| arr (SI (pure . R1) fromR))
    where
      fromL (L1 a) = pure a
      fromL _ = empty
      fromR (R1 b) = pure b
      fromR _ = empty
