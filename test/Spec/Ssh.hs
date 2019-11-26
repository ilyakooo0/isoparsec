{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.Ssh
  ( spec,
    quickSpec,
  )
where

import Data.ByteString as BS
import qualified Data.Char as C
import Data.Isoparsec
import Data.Isoparsec.ByteString
import Data.Isoparsec.Printer
import Data.Maybe
import qualified Data.Word8 as W8
import GHC.Generics
import Optics
import Spec.Helper
import Spec.Orphans ()
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude as P hiding ((.))

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

instance ToIsoparsec MessageNumber ByteString where
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

instance ToIsoparsec DisconnectReasonCode ByteString

newtype AlwaysDisplay = AlwaysDisplay {unAlwaysDisplay :: Bool}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec AlwaysDisplay ByteString

newtype PacketSequenceNumber
  = PacketSequenceNumber {unPacketSequenceNumber :: Byte32 'BE}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec PacketSequenceNumber ByteString

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

instance ToIsoparsec Payload ByteString where
  toIsoparsec =
    try
      ( _DisconnectPayload
          <.> specific DisconnectMsg
            &&& auto @DisconnectReasonCode
            &&& (tokensWhile (const True) >>> ftu8)
      )
      <+> try
        ( _ServiceRequest <.> specific ServiceRequestMsg
            &&& auto @SSHString
        )
      <+> try
        ( _VersionPayload <.> utf8 "SSH-2.0-"
            &&& ( ( try (takeUntil "\r\n" >>> check (BS.all (not . W8.isSpace)))
                      <+> ((takeUntil " " &&& (takeUntil "\r\n" >>> badTsnok "")) >>% morphed)
                  )
                    >>> ftu8
                )
        )
      <+> try (_IgnorePayload <.> specific IgnoreMsg &&& tokensWhile (const True))
      <+> try
        ( _DebugPayload <.> specific DebugMsg
            &&& auto @AlwaysDisplay
            &&& auto @SSHString
        )
      <+> try
        ( _UnimplementedPayload <.> specific UnimplementedMsg
            &&& auto @PacketSequenceNumber
        )
      <+> try
        ( _ServiceAccept <.> specific ServiceAcceptMsg
            &&& auto @SSHString
        )

newtype Padding = Padding {unPadding :: ByteString}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

makePrisms ''Padding

newtype MAC = MAC {unMAC :: ByteString}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

data NoneMAC = NoneMAC
  deriving (Eq, Ord, Show, Generic)

instance ToIsoparsec NoneMAC b where
  toIsoparsec = konst NoneMAC

instance Arbitrary NoneMAC where
  arbitrary = return NoneMAC

data Packet mac
  = Packet Payload Padding mac
  deriving (Eq, Show)

instance Arbitrary mac => Arbitrary (Packet mac) where
  arbitrary = Packet <$> arbitrary <*> arbitrary <*> arbitrary

makePrisms ''Packet

instance ToIsoparsec mac ByteString => ToIsoparsec (Packet mac) ByteString where
  toIsoparsec =
    ( ( (auto @(Byte32 'BE) &&& auto @Byte8)
          >>> (throughIntegral *** throughIntegral)
          >>> siJust
            (\(packetL, paddingL) -> (packetL - paddingL - 1, paddingL))
            (\(payloadL, paddingL) -> (payloadL + paddingL + 1, paddingL))
          ^>> (manyTokens *** manyTokens) >>> (tuck (auto @Payload) *** coercing @Padding)
      )
        &&& auto @mac
    )
      >>^ siJust (\((a, b), c) -> Packet a b c) (\(Packet a b c) -> ((a, b), c))

spec :: Spec
spec = do
  runIO . (print :: ByteString -> IO ()) . fromJust . runPrinter auto $ Packet (ServiceRequest (SSHString "henlo")) (Padding "69") NoneMAC
  it "deserialize payload" $ do
    "SSH-2.0-TesT\r\n" `shouldParseBS` VersionPayload "TesT"
    "SSH-2.0-TesT random comment\r\n" `shouldParseBS` VersionPayload "TesT"
    "\x2__" `shouldParseBS` IgnorePayload "__"
    "\x5\0\0\0\x6tested" `shouldParseBS` ServiceRequest (SSHString "tested")
  it "deserialize packet" $
    ("\0\0\0\xd" <> "\x2" <> "\x5\0\0\0\x5henlo" <> "69")
      `shouldParseBS` Packet (ServiceRequest (SSHString "henlo")) (Padding "69") NoneMAC

quickSpec :: TestTree
quickSpec =
  testGroup
    "roundtrips"
    [ testProperty "payload" $ roundtrip @Payload @ByteString,
      testProperty "packet" $ roundtrip @(Packet NoneMAC) @ByteString
    ]
