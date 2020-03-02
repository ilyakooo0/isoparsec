{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.ByteString
  ( utf8,
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

utf8 :: PolyArrow SemiIso m => m ByteString String
utf8 = arr $ siPure C.unpack C.pack

data Endianness = BE | LE

class BytesToIsoparsec b (e :: Endianness) where
  bytesToIsoparsec :: (Isoparsec m ByteString) => Proxy e -> m () b

instance (FiniteBits b, Integral b) => BytesToIsoparsec b 'BE where
  bytesToIsoparsec _ =
    konst word8s ^>> manyTokens
      >>^ siPure
        (fromInteger . BS.foldl (\i w -> shiftL i 8 .|. toInteger w) 0)
        ( fromList . snd . P.head
            . P.drop (fromIntegral word8s)
            . iterate (\(i, ww) -> (shiftR i 8, fromInteger i : ww))
            . (,[])
            . toInteger
        )
    where
      word8s = fromIntegral $ finiteBitSize (undefined :: b) `div` 8

instance (FiniteBits b, Integral b) => BytesToIsoparsec b 'LE where
  bytesToIsoparsec _ =
    konst word8s ^>> manyTokens
      >>^ siPure
        (fromInteger . BS.foldr (\w i -> shiftL i 8 .|. toInteger w) 0)
        ( fromList . P.reverse . snd . P.head
            . P.drop (fromIntegral word8s)
            . iterate (\(i, ww) -> (shiftR i 8, fromInteger i : ww))
            . (,[])
            . toInteger
        )
    where
      word8s = fromIntegral $ finiteBitSize (undefined :: b) `div` 8

type Byte8 = Word8

newtype Byte16 (e :: Endianness) = Byte16 {unByte16 :: Word16}
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bounded)

newtype Byte32 (e :: Endianness) = Byte32 {unByte32 :: Word32}
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bounded)

newtype Byte64 (e :: Endianness) = Byte64 {unByte64 :: Word64}
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bounded)

instance BytesToIsoparsec Word16 e => ToIsoparsec (Byte16 e) ByteString m where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>^ coercing @(Byte16 e) @Word16

instance BytesToIsoparsec Word32 e => ToIsoparsec (Byte32 e) ByteString m where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>^ coercing @(Byte32 e) @Word32

instance BytesToIsoparsec Word64 e => ToIsoparsec (Byte64 e) ByteString m where
  toIsoparsec = bytesToIsoparsec (Proxy @e) >>^ coercing @(Byte64 e) @Word64

instance ToIsoparsec Bool ByteString m where
  toIsoparsec = anyToken >>> mapIso [(0, False), (1, True)]

newtype SSHString = SSHString {unSSHString :: String}
  deriving (Show, Eq, Ord)

instance ToIsoparsec SSHString ByteString m where
  toIsoparsec =
    auto @(Byte32 'BE)
      >>^ coercing @Word32
      >>^ siPure fromIntegral fromIntegral
      >>> manyTokens
      >>> utf8
      >>^ siPure SSHString unSSHString
