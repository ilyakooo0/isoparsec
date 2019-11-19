{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Isoparsec.ToIsoparsec
  ( ToIsoparsec (..),
  )
where

import Control.Arrow.Extra
import Data.Isoparsec.Internal
import Data.Proxy
import GHC.Generics
import Prelude hiding ((.))

class ToIsoparsec s a where

  toIsoparsec :: Isoparsec m s => Proxy a -> m () a

  default toIsoparsec :: (Isoparsec m s, Generic a, GToIsoparsec s (Rep a)) => Proxy a -> m () a
  toIsoparsec (Proxy :: Proxy a) = gToIsoparsec (Proxy @(Rep a)) >>^ si' (Just . to) (Just . from)

class GToIsoparsec s a where
  gToIsoparsec :: Isoparsec m s => Proxy a -> m () (a b)

instance GToIsoparsec s U1 where
  gToIsoparsec _ = konst U1

instance ToIsoparsec s c => GToIsoparsec s (K1 i c) where
  gToIsoparsec (Proxy :: Proxy (K1 i c)) =
    toIsoparsec (Proxy @c) >>^ si' (Just . K1) (Just . unK1)

instance GToIsoparsec s c => GToIsoparsec s (M1 i t c) where
  gToIsoparsec (Proxy :: Proxy (M1 i t c)) =
    gToIsoparsec (Proxy @c) >>^ si' (Just . M1) (Just . unM1)

instance (GToIsoparsec s a, GToIsoparsec s b) => GToIsoparsec s (a :*: b) where
  gToIsoparsec (Proxy :: Proxy (a :*: b)) =
    (gToIsoparsec (Proxy @a) &&& gToIsoparsec (Proxy @b))
      >>^ si' (\(a, b) -> Just (a :*: b)) (\(a :*: b) -> Just (a, b))

instance (GToIsoparsec s a, GToIsoparsec s b) => GToIsoparsec s (a :+: b) where
  gToIsoparsec (Proxy :: Proxy (a :+: b)) =
    ( ( try (gToIsoparsec (Proxy @a) >>^ si' (Just . Left) fromLeft)
          <+> (gToIsoparsec (Proxy @b) >>^ si' (Just . Right) fromRight)
      )
        >>> ((arr $ si' (Just . L1) fromL) ||| (arr $ si' (Just . R1) fromR))
    )
    where
      fromLeft (Left a) = Just a
      fromLeft _ = Nothing
      fromRight (Right b) = Just b
      fromRight _ = Nothing
      fromL (L1 a) = Just a
      fromL _ = Nothing
      fromR (R1 b) = Just b
      fromR _ = Nothing
