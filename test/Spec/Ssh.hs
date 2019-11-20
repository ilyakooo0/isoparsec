{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Ssh
  ( spec,
    quickSpec,
  )
where

import Data.ByteString.Char8 as C
import Data.Char
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.ByteString
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Data.Void
import Data.Word
import GHC.Generics
import Optics
import Spec.Orphans ()
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Megaparsec.Error
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
  = DisconnectReasonCode {unDisconnectReasonCode :: Word32}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec DisconnectReasonCode ByteString

newtype AlwaysDisplay = AlwaysDisplay {unAlwaysDisplay :: Bool}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec AlwaysDisplay ByteString

newtype PacketSequenceNumber
  = PacketSequenceNumber {unPacketSequenceNumber :: Word32}
  deriving (Eq, Ord, Show, Generic, Arbitrary)

instance ToIsoparsec PacketSequenceNumber ByteString

data Payload
  = VersionPayload String
  | IgnorePayload ByteString
  | ServiceRequest String
  | DebugPayload AlwaysDisplay String
  | DisconnectPayload DisconnectReasonCode String
  | ServiceAccept String
  | UnimplementedPayload PacketSequenceNumber
  deriving (Show, Eq)

instance Arbitrary Payload where
  arbitrary =
    oneof
      [ VersionPayload . P.filter (not . isSpace) <$> s,
        IgnorePayload <$> arbitrary,
        ServiceRequest <$> s,
        DebugPayload <$> arbitrary <*> s,
        DisconnectPayload <$> arbitrary <*> s,
        ServiceAccept <$> s,
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
            &&& auto @String
        )
      <+> try
        ( _VersionPayload <.> utf8 "SSH-2.0-"
            &&& (tokensWhile ((\c -> c /= ' ' && c /= '\r') . toEnum . fromEnum) >>> ftu8)
            &&& opt (chunk " " &&& (tokensWhile (/= (toEnum . fromEnum $ '\r')) >>> badTsnok "") >>% morphed)
            &&& chunk "\r\n"
        )
      <+> try (_IgnorePayload <.> specific IgnoreMsg &&& tokensWhile (const True))
      <+> try
        ( _DebugPayload <.> specific DebugMsg
            &&& auto @AlwaysDisplay
            &&& auto @String
        )
      <+> try
        ( _UnimplementedPayload <.> specific UnimplementedMsg
            &&& auto @PacketSequenceNumber
        )
      <+> try
        ( _ServiceAccept <.> specific ServiceAcceptMsg
            &&& auto @String
        )

spec :: Spec
spec = do
  let parser = toIsoparsec @Payload
  let shouldParse s e = runMegaparsec @() @ByteString parser s `shouldBe` Right e
  it "deserializes" $ do
    "SSH-2.0-TesT\r\n" `shouldParse` VersionPayload "TesT"
    "\x2__" `shouldParse` IgnorePayload "__"
    "\x5\0\0\0\x6tested" `shouldParse` ServiceRequest "tested"

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \(x :: Payload) ->
  let s = fromJust $ runPrinter @Maybe @ByteString toIsoparsec x
   in counterexample (C.unpack s) $ case runMegaparsec @Void @ByteString toIsoparsec s of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
