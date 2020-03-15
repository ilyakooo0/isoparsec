module Data.Isoparsec.ToIsoparsec
  ( ToIsoparsec (..),
  )
where

import Control.Arrow.Extra
import Data.Isoparsec.Internal
import GHC.Generics
import Prelude hiding ((.))

class ToIsoparsec a s m where
  toIsoparsec :: (Isoparsec m s) => m () a
  default toIsoparsec :: (Isoparsec m s, Generic a, GToIsoparsec (Rep a) s m) => m () a
  toIsoparsec = gToIsoparsec >>^ SI (pure . to) (pure . from)

class GToIsoparsec a s m where
  gToIsoparsec :: Isoparsec m s => m () (a b)

instance GToIsoparsec U1 s m where
  gToIsoparsec = arr $ konst U1

instance ToIsoparsec c s m => GToIsoparsec (K1 i c) s m where
  gToIsoparsec = toIsoparsec >>^ siPure K1 unK1

instance GToIsoparsec c s m => GToIsoparsec (M1 i t c) s m where
  gToIsoparsec =
    gToIsoparsec >>^ siPure M1 unM1

instance (GToIsoparsec a s m, GToIsoparsec b s m) => GToIsoparsec (a :*: b) s m where
  gToIsoparsec =
    (gToIsoparsec &&& gToIsoparsec)
      >>^ SI (\(a, b) -> pure (a :*: b)) (\(a :*: b) -> pure (a, b))

instance (GToIsoparsec a s m, GToIsoparsec b s m) => GToIsoparsec (a :+: b) s m where
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
