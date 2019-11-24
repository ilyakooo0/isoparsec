{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.ByteString
  ( utf8,
    ftu8,
    ByteString,
    Endianness (..),
    Byte8,
    Byte16 (..),
    Byte32 (..),
    Byte64 (..),
    SSHString (..),
  )
where

import Data.Bits
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Isoparsec
import Data.Proxy
import Data.Word
import Prelude as P hiding ((.))

utf8 :: Isoparsec m ByteString => String -> m () ()
utf8 = chunk . C.pack

ftu8 :: PolyArrow m SemiIso' => m ByteString String
ftu8 = arr $ siJust C.unpack C.pack

data Endianness = BE | LE

class BytesToIsoparsec b (e :: Endianness) where
  bytesToIsoparsec :: (Isoparsec m ByteString) => Proxy e -> m () b

instance (FiniteBits b, Integral b) => BytesToIsoparsec b 'BE where
  bytesToIsoparsec _ =
    konst word8s >>> manyTokens
      >>^ siJust
        (fromInteger . BS.foldl (\i w -> shiftL i 8 .|. toInteger w) 0)
        ( liftTokens . snd . P.head
            . P.drop (fromIntegral word8s)
            . iterate (\(i, ww) -> (shiftR i 8, fromInteger i : ww))
            . (,[])
            . toInteger
        )
    where
      word8s = fromIntegral $ finiteBitSize (undefined :: b) `div` 8

instance (FiniteBits b, Integral b) => BytesToIsoparsec b 'LE where
  bytesToIsoparsec _ =
    konst word8s >>> manyTokens
      >>^ siJust
        (fromInteger . BS.foldr (\w i -> shiftL i 8 .|. toInteger w) 0)
        ( liftTokens . P.reverse . snd . P.head
            . P.drop (fromIntegral word8s)
            . iterate (\(i, ww) -> (shiftR i 8, fromInteger i : ww))
            . (,[])
            . toInteger
        )
    where
      word8s = fromIntegral $ finiteBitSize (undefined :: b) `div` 8

type Byte8 = Word8

newtype Byte16 (e :: Endianness) = Byte16 {unByte16 :: Word16}
  deriving (Show, Eq, Ord, Enum, Num, Bounded)

newtype Byte32 (e :: Endianness) = Byte32 {unByte32 :: Word32}
  deriving (Show, Eq, Ord, Enum, Num, Bounded)

newtype Byte64 (e :: Endianness) = Byte64 {unByte64 :: Word64}
  deriving (Show, Eq, Ord, Enum, Num, Bounded)

instance BytesToIsoparsec Word16 e => ToIsoparsec (Byte16 e) ByteString where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>> coercing @Word16 @(Byte16 e)

instance BytesToIsoparsec Word32 e => ToIsoparsec (Byte32 e) ByteString where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>> coercing @Word32 @(Byte32 e)

instance BytesToIsoparsec Word64 e => ToIsoparsec (Byte64 e) ByteString where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>> coercing @Word64 @(Byte64 e)

instance ToIsoparsec Bool ByteString where
  toIsoparsec = anyToken >>> mapIso [(0, False), (1, True)]

newtype SSHString = SSHString {unSSHString :: String}
  deriving (Show, Eq, Ord)

instance ToIsoparsec SSHString ByteString where
  toIsoparsec =
    auto @(Byte32 'BE) >>> coercing @_ @Word32
      >>> siJust fromIntegral fromIntegral ^>> manyTokens
      >>> ftu8
      >>^ siJust SSHString unSSHString

instance Tokenable ByteString where

  type Token ByteString = Word8

  liftTokens = pack

  lowerTokens = unpack
