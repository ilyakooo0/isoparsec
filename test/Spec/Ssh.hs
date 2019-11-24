{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import Data.ByteString as BS
import Data.ByteString.Char8 as C
import qualified Data.Char as C
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.ByteString
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Data.Void
import qualified Data.Word8 as W8
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

spec :: Spec
spec = do
  let parser = toIsoparsec @Payload
  let shouldParse s e = runMegaparsec @() @ByteString parser s `shouldBe` Right e
  it "deserializes" $ do
    "SSH-2.0-TesT\r\n" `shouldParse` VersionPayload "TesT"
    "SSH-2.0-TesT random comment\r\n" `shouldParse` VersionPayload "TesT"
    "\x2__" `shouldParse` IgnorePayload "__"
    "\x5\0\0\0\x6tested" `shouldParse` ServiceRequest (SSHString "tested")

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \(x :: Payload) ->
  let s = fromJust $ runPrinter @Maybe @ByteString toIsoparsec x
   in counterexample (C.unpack s) $ case runMegaparsec @Void @ByteString toIsoparsec s of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
