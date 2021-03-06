{-# LANGUAGE TemplateHaskell #-}

module Spec.Ssh
  ( spec,
    quickSpec,
  )
where

import Control.Lens.TH
import Data.ByteString as BS
import qualified Data.Char as C
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.ByteString
import GHC.Generics
import Spec.Helper
import Spec.Orphans ()
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
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
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec DisconnectReasonCode ByteString a

newtype AlwaysDisplay = AlwaysDisplay {unAlwaysDisplay :: Bool}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec AlwaysDisplay ByteString a

newtype PacketSequenceNumber
  = PacketSequenceNumber {unPacketSequenceNumber :: Byte32 'BE}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

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

instance Arbitrary Payload where
  arbitrary =
    oneof
      [ VersionPayload . P.filter (not . C.isSpace) <$> s,
        IgnorePayload <$> arbitrary,
        ServiceRequest . SSHString <$> s,
        DebugPayload <$> arbitrary <*> (SSHString <$> s),
        DisconnectPayload <$> arbitrary <*> s,
        ServiceAccept . SSHString <$> s,
        UnimplementedPayload <$> arbitrary
      ]
    where
      s = getASCIIString <$> arbitrary

makePrisms ''Payload

instance ToIsoparsec Payload ByteString a where
  toIsoparsec =
    ( _DisconnectPayload
        <.> specific DisconnectMsg
          &&& auto @DisconnectReasonCode
          &&& (tokensWhile (const True) >>^ utf8)
    )
      <+> ( _ServiceRequest <.> specific ServiceRequestMsg
              &&& auto @SSHString
          )
      <+> ( _VersionPayload <.> chunk "SSH-2.0-"
              &&& (tokensWhile (`BS.notElem` " \n\r") >>^ utf8)
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
  deriving (Eq, Ord, Show, Generic, Arbitrary)

newtype ZeroPadding = ZeroPadding {zeroPaddingLength :: Byte8}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

badZeroPadding :: Isoparsec m ByteString => m Byte8 ZeroPadding
badZeroPadding =
  throughIntegral
    ^>> manyTokens
    >>^ siPure
      (ZeroPadding . fromIntegral . BS.length)
      (flip BS.replicate 0 . fromIntegral . zeroPaddingLength)

newtype MAC = MAC {unMAC :: ByteString}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

data NoneMAC = NoneMAC
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec NoneMAC b a where
  toIsoparsec = arr $ konst NoneMAC

instance Arbitrary NoneMAC where
  arbitrary = return NoneMAC

data Packet mac
  = Packet Payload ZeroPadding mac
  deriving (Eq, Show)

instance Arbitrary mac => Arbitrary (Packet mac) where
  arbitrary = Packet <$> arbitrary <*> arbitrary <*> arbitrary

makePrisms ''Packet

instance ToIsoparsec mac ByteString a => ToIsoparsec (Packet mac) ByteString a where
  toIsoparsec =
    ( auto @(Byte32 'BE) &&& anyToken
        >>^ throughIntegral *** throughIntegral
        >>^ siPure
          (\(packetL, paddingL) -> (packetL - paddingL - 1, paddingL))
          (\(payloadL, paddingL) -> (payloadL + paddingL + 1, paddingL))
        >>> manyTokens *** arr throughIntegral
        >>> tuck (auto @Payload) *** badZeroPadding
    )
      &&& auto @mac
      >>^ siPure (\((a, b), c) -> Packet a b c) (\(Packet a b c) -> ((a, b), c))

spec :: Spec
spec = do
  it "deserialize payload" $ do
    "SSH-2.0-TesT\r\n" `shouldParseBS` VersionPayload "TesT"
    "SSH-2.0-TesT" `parseSatisfyBS` isLeft @_ @Payload
    "SSH-2.0-TesT random comment\r\n" `shouldParseBS` VersionPayload "TesT"
    "SSH-2.0-TesT random comment" `parseSatisfyBS` isLeft @_ @Payload
    "\x2__" `shouldParseBS` IgnorePayload "__"
    "\x5\0\0\0\x6tested" `shouldParseBS` ServiceRequest (SSHString "tested")
  it "deserialize packet" $ do
    ("\0\0\0\xd" <> "\x2" <> "\x5\0\0\0\x5henlo" <> "69")
      `shouldParseBS` Packet (ServiceRequest (SSHString "henlo")) (ZeroPadding 2) NoneMAC
    ("\0\0\0\xd" <> "\x2" <> "\x2\0\0\0\x5henlo" <> "69")
      `shouldParseBS` Packet (IgnorePayload "\0\0\0\x5henlo") (ZeroPadding 2) NoneMAC
    ("\0\0\0\xd" <> "\x3" <> "\x2\0\0henlo!" <> "69a")
      `shouldParseBS` Packet (IgnorePayload "\0\0henlo!") (ZeroPadding 3) NoneMAC
    ("\0\0\0\xb" <> "\x3" <> "\x2henlo!" <> "69a")
      `shouldParseBS` Packet (IgnorePayload "henlo!") (ZeroPadding 3) NoneMAC

quickSpec :: TestTree
quickSpec =
  testGroup
    "roundtrips"
    [ testProperty "payload" $ roundtrip @Payload @ByteString,
      testProperty "packet" $ roundtrip @(Packet NoneMAC) @ByteString
    ]
