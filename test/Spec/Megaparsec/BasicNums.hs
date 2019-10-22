{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Megaparsec.BasicNums
  ( spec,
    quickSpec,
  )
where

import Data.Either
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Data.Void
import Optics
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Megaparsec.Error
import Prelude hiding ((.), fail)

data Foo = Foo Integer Integer
  deriving (Show, Eq)

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary <*> arbitrary

makePrisms ''Foo

parser :: forall m. (Isoparsec m '[Char] '[String], IsoparsecLabel m String) => m () Foo
parser = (_Foo <.> (number &&& whiteSpace1 &&& number <?> "oh no"))

spec :: Spec
spec =
  it "deserializes" $ do
    runMegaparsec @() parser "12 31" `shouldBe` (Right $ Foo 12 31)
    runMegaparsec @() parser "1   33" `shouldBe` (Right $ Foo 1 33)
    runMegaparsec @() parser "1562" `shouldSatisfy` isLeft

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \x ->
  let s = fromJust $ runPrinter @Maybe @String parser x
   in counterexample s $ case runMegaparsec @Void parser s of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
