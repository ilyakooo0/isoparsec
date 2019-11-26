{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.Helper
  ( shouldParse,
    shouldParseBS,
    roundtrip,
  )
where

import Data.ByteString as BS
import Data.Either
import Data.Isoparsec as I
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Data.Void
import Spec.Orphans ()
import Test.Hspec
import Test.Tasty.QuickCheck
import Text.Megaparsec as M
import Prelude as P hiding ((.))

shouldParse ::
  forall x s.
  ( ToIsoparsec x s,
    Stream s,
    Show s,
    Show (I.Token s),
    s ~ M.Tokens s,
    I.Token s ~ M.Token s,
    Show x,
    Eq x,
    Isoparsec (Kleisli (Parsec Void s)) s
  ) =>
  s ->
  x ->
  Expectation
shouldParse s e = runMegaparsec @Void @s toIsoparsec s `shouldBe` Right e

shouldParseBS ::
  forall x.
  ( ToIsoparsec x ByteString,
    Show x,
    Eq x,
    Isoparsec (Kleisli (Parsec Void ByteString)) ByteString
  ) =>
  ByteString ->
  x ->
  Expectation
shouldParseBS = shouldParse

roundtrip ::
  forall x s.
  ( ToIsoparsec x s,
    Stream s,
    Show s,
    I.Token s ~ M.Token s,
    Eq x,
    Isoparsec (Kleisli (Parsec Void s)) s
  ) =>
  x ->
  Property
roundtrip x =
  let s = fromJust $ runPrinter @Maybe @s toIsoparsec x
   in counterexample (show s) $ case runMegaparsec @Void @s toIsoparsec s of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
