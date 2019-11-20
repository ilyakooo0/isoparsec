{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  )
where

import Data.Bits
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Isoparsec
import Data.Word
import Prelude as P hiding ((.))

utf8 :: Isoparsec m ByteString => String -> m () ()
utf8 = chunk . C.pack

ftu8 :: PolyArrow m SemiIso' => m ByteString String
ftu8 = arr $ siJust C.unpack C.pack

-- | Big-endian
bytesToIsoparsec :: forall b m. (FiniteBits b, Integral b, Isoparsec m ByteString) => m () b
bytesToIsoparsec =
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

instance ToIsoparsec Word16 ByteString where
  toIsoparsec = bytesToIsoparsec

instance ToIsoparsec Word32 ByteString where
  toIsoparsec = bytesToIsoparsec

instance ToIsoparsec Word64 ByteString where
  toIsoparsec = bytesToIsoparsec

instance ToIsoparsec Bool ByteString where
  toIsoparsec = anyToken >>> mapIso [(0, False), (1, True)]

instance ToIsoparsec String ByteString where
  toIsoparsec =
    auto @Word32
      >>> siJust fromIntegral fromIntegral ^>> manyTokens
      >>> ftu8

instance Tokenable ByteString where

  type Token ByteString = Word8

  liftTokens = pack

  lowerTokens = unpack
