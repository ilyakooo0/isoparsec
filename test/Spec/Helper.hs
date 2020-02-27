module Spec.Helper
  ( shouldParseBS,
    roundtrip,
    parseSatisfyBS,
    shouldParseS,
  )
where

import Control.Cokleisli
import Control.Monad.Writer.Lazy
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
  ( ToIsoparsec x s (Kleisli (Parsec Void s)),
    Stream s,
    Show x,
    Show (M.Token s),
    Show s,
    Isoparsec (Kleisli (Parsec Void s)) s
  ) =>
  s ->
  (Either (ParseErrorBundle s Void) x -> Bool) ->
  Expectation
parseSatisfy s p = runMegaparsec @Void @s s toIsoparsec `shouldSatisfy` p

parseSatisfyBS ::
  forall x.
  ( ToIsoparsec x ByteString (Kleisli (Parsec Void ByteString)),
    Show x,
    Isoparsec (Kleisli (Parsec Void ByteString)) ByteString
  ) =>
  ByteString ->
  (Either (ParseErrorBundle ByteString Void) x -> Bool) ->
  Expectation
parseSatisfyBS = parseSatisfy

shouldParse ::
  forall x s.
  ( ToIsoparsec x s (Kleisli (Parsec Void s)),
    Stream s,
    Show x,
    Eq x,
    Isoparsec (Kleisli (Parsec Void s)) s
  ) =>
  s ->
  x ->
  Expectation
shouldParse s e = case runMegaparsec @Void @s s toIsoparsec of
  Right e' -> e' `shouldBe` e
  Left err -> expectationFailure $ errorBundlePretty err

shouldParseBS ::
  forall x.
  ( ToIsoparsec x ByteString (Kleisli (Parsec Void ByteString)),
    Show x,
    Eq x,
    Isoparsec (Kleisli (Parsec Void ByteString)) ByteString
  ) =>
  ByteString ->
  x ->
  Expectation
shouldParseBS = shouldParse

shouldParseS ::
  forall x.
  ( ToIsoparsec x String (Kleisli (Parsec Void String)),
    Show x,
    Eq x,
    Isoparsec (Kleisli (Parsec Void String)) String
  ) =>
  String ->
  x ->
  Expectation
shouldParseS = shouldParse

roundtrip ::
  forall x s.
  ( ToIsoparsec x s (Kleisli (Parsec Void s)),
    ToIsoparsec x s (Cokleisli (WriterT (Dual s) Maybe)),
    Stream s,
    Show s,
    I.Element s ~ M.Token s,
    Eq x,
    Eq s,
    Isoparsec (Kleisli (Parsec Void s)) s
  ) =>
  x ->
  Property
roundtrip x =
  let s = fromJust $ runPrinter @Maybe @s toIsoparsec x
   in counterexample (show s) $ case runMegaparsec @Void @s s toIsoparsec of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
