module Spec.Helper
  ( shouldParseBS,
    roundtrip,
    parseSatisfyBS,
    shouldParseS,
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

parseSatisfy ::
  forall x s.
  ( ToIsoparsec x s (MegaparsecParser s),
    Stream s,
    Show x,
    Show (M.Token s),
    Show s,
    Isoparsec (MegaparsecParser s) s
  ) =>
  s ->
  (Either (ParseErrorBundle s Void) x -> Bool) ->
  Expectation
parseSatisfy s p = runMegaparsecParser @s s toIsoparsec `shouldSatisfy` p

parseSatisfyBS ::
  forall x.
  ( ToIsoparsec x ByteString (MegaparsecParser ByteString),
    Show x,
    Isoparsec (MegaparsecParser ByteString) ByteString
  ) =>
  ByteString ->
  (Either (ParseErrorBundle ByteString Void) x -> Bool) ->
  Expectation
parseSatisfyBS = parseSatisfy

shouldParse ::
  forall x s.
  ( ToIsoparsec x s (MegaparsecParser s),
    Stream s,
    Show x,
    Eq x,
    Isoparsec (MegaparsecParser s) s
  ) =>
  s ->
  x ->
  Expectation
shouldParse s e = case runMegaparsecParser @s s toIsoparsec of
  Right e' -> e' `shouldBe` e
  Left err -> expectationFailure $ errorBundlePretty err

shouldParseBS ::
  forall x.
  ( ToIsoparsec x ByteString (MegaparsecParser ByteString),
    Show x,
    Eq x,
    Isoparsec (MegaparsecParser ByteString) ByteString
  ) =>
  ByteString ->
  x ->
  Expectation
shouldParseBS = shouldParse

shouldParseS ::
  forall x.
  ( ToIsoparsec x String (MegaparsecParser String),
    Show x,
    Eq x,
    Isoparsec (MegaparsecParser String) String
  ) =>
  String ->
  x ->
  Expectation
shouldParseS = shouldParse

roundtrip ::
  forall x s.
  ( ToIsoparsec x s (MegaparsecParser s),
    ToIsoparsec x s (MonoidPrinter s),
    Stream s,
    Show s,
    I.Element s ~ M.Token s,
    Eq x,
    Isoparsec (MegaparsecParser s) s
  ) =>
  x ->
  Property
roundtrip x =
  let s = fromJust $ runMonoidPrinter @s toIsoparsec x
   in counterexample (show s) $ case runMegaparsecParser @s s toIsoparsec of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
