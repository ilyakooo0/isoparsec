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
import GHC.Generics
import Prelude hiding ((.))

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
    ( ( try (gToIsoparsec >>^ si' (Just . Left) fromLeft)
          <+> (gToIsoparsec >>^ si' (Just . Right) fromRight)
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
