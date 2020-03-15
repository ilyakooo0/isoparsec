{-# LANGUAGE TemplateHaskell #-}

module Spec.Megaparsec.BasicNums
  ( spec,
    quickSpec,
  )
where

import Control.Lens.TH
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
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

parser :: (Isoparsec m String) => m () Foo
parser = _Foo <.> (number &&& unsafeWhiteSpace1 &&& number)

spec :: Spec
spec =
  it "deserializes" $ do
    runMegaparsecParser "12 31" parser `shouldBe` Right (Foo 12 31)
    runMegaparsecParser "1   33" parser `shouldBe` Right (Foo 1 33)
    runMegaparsecParser "1562" parser `shouldSatisfy` isLeft

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \x ->
  let s = fromJust $ runMonoidPrinter @String parser x
   in counterexample s $ case runMegaparsecParser s parser of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
