{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Control.Lens.TH
import Data.ByteString as BS
import qualified Data.Char as C
import Data.Isoparsec
import Data.Isoparsec.ByteString
import Data.Isoparsec.Debugger
import GHC.Generics
import Prelude as P hiding ((.), id)

data MessageNumber
  = DisconnectMsg
  | IgnoreMsg
  | UnimplementedMsg
  | DebugMsg
  | ServiceRequestMsg
  | ServiceAcceptMsg
  | KextInitMsg
  | NewKeysMsg
  deriving (Eq, Show, Ord)

instance ToIsoparsec MessageNumber ByteString a where
  toIsoparsec =
    anyToken
      >>> mapIso
        [ (1, DisconnectMsg),
          (2, IgnoreMsg),
          (3, UnimplementedMsg),
          (4, DebugMsg),
          (5, ServiceRequestMsg),
          (6, ServiceAcceptMsg),
          (20, KextInitMsg),
          (21, NewKeysMsg)
        ]

newtype DisconnectReasonCode
  = DisconnectReasonCode {unDisconnectReasonCode :: Byte32 'BE}
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec DisconnectReasonCode ByteString a

newtype AlwaysDisplay = AlwaysDisplay {unAlwaysDisplay :: Bool}
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec AlwaysDisplay ByteString a

newtype PacketSequenceNumber
  = PacketSequenceNumber {unPacketSequenceNumber :: Byte32 'BE}
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec PacketSequenceNumber ByteString a

data Payload
  = VersionPayload String
  | IgnorePayload ByteString
  | ServiceRequest SSHString
  | DebugPayload AlwaysDisplay SSHString
  | DisconnectPayload DisconnectReasonCode String
  | ServiceAccept SSHString
  | UnimplementedPayload PacketSequenceNumber
  deriving (Show, Eq)

makePrisms ''Payload

instance ToIsoparsec Payload ByteString a where
  toIsoparsec =
    ( _DisconnectPayload
        <.> specific DisconnectMsg
          &&& auto @DisconnectReasonCode
          &&& (tokensWhile (const True) >>> utf8)
    )
      <+> ( _ServiceRequest <.> specific ServiceRequestMsg
              &&& auto @SSHString
          )
      <+> ( _VersionPayload <.> chunk "SSH-2.0-"
              &&& (tokensWhile (`BS.notElem` " \n\r") >>> utf8)
              &&& ( (chunk " " >>> takeUntil "\r\n" >>^ (maskr . turn . konst $ ""))
                      <+> chunk "\r\n"
                  )
          )
      <+> (_IgnorePayload <.> specific IgnoreMsg &&& tokensWhile (const True))
      <+> ( _DebugPayload <.> specific DebugMsg
              &&& auto @AlwaysDisplay
              &&& auto @SSHString
          )
      <+> ( _UnimplementedPayload <.> specific UnimplementedMsg
              &&& auto @PacketSequenceNumber
          )
      <+> ( _ServiceAccept <.> specific ServiceAcceptMsg
              &&& auto @SSHString
          )

newtype Padding = Padding {unPadding :: ByteString}
  deriving (Eq, Ord, Show, Generic)

newtype ZeroPadding = ZeroPadding {zeroPaddingLength :: Byte8}
  deriving (Eq, Ord, Show, Generic)

badZeroPadding :: Isoparsec m ByteString => m Byte8 ZeroPadding
badZeroPadding =
  throughIntegral
    >>> manyTokens
    >>^ siPure
      (ZeroPadding . fromIntegral . BS.length)
      (flip BS.replicate 0 . fromIntegral . zeroPaddingLength)

newtype MAC = MAC {unMAC :: ByteString}
  deriving (Eq, Ord, Show, Generic)

data NoneMAC = NoneMAC
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec NoneMAC b a where
  toIsoparsec = arr $ konst NoneMAC

data Packet mac
  = Packet Payload ZeroPadding mac
  deriving (Eq, Show)

makePrisms ''Packet

instance ToIsoparsec mac ByteString a => ToIsoparsec (Packet mac) ByteString a where
  toIsoparsec =
    ( auto @(Byte32 'BE) &&& auto @Byte8
        >>> throughIntegral *** throughIntegral
        >>^ siPure
          (\(packetL, paddingL) -> (packetL - paddingL - 1, paddingL))
          (\(payloadL, paddingL) -> (payloadL + paddingL + 1, paddingL))
        >>> manyTokens *** throughIntegral
        >>> tuck (auto @Payload) *** badZeroPadding
    )
      &&& auto @mac
      >>^ siPure (\((a, b), c) -> Packet a b c) (\(Packet a b c) -> ((a, b), c))

main :: IO ()
main = debug @ByteString (auto @(Packet NoneMAC))
