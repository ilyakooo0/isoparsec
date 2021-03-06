module Spec.TwoDigits
  ( spec,
    quickSpec,
  )
where

import Data.Char
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.Chunks
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import GHC.Generics
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Megaparsec.Error
import Prelude hiding ((.))

newtype SingleDigit = SingleDigit {unSingleDigit :: String}
  deriving (Eq, Show, Generic)

instance ToIsoparsec SingleDigit String m where
  toIsoparsec =
    toIsoparsec @(Chunk 1 String)
      >>^ coercing
      >>^ check (all @[] isDigit)
      >>^ coercing

instance Arbitrary SingleDigit where
  arbitrary = SingleDigit . pure <$> elements ['0' .. '9']

data Digits
  = FourDigits SingleDigit SingleDigit SingleDigit SingleDigit
  | ThreeDigits SingleDigit SingleDigit SingleDigit
  | TwoDigits SingleDigit SingleDigit
  deriving (Generic, Show, Eq)

instance Arbitrary Digits where
  arbitrary =
    oneof
      [ FourDigits <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        ThreeDigits <$> arbitrary <*> arbitrary <*> arbitrary,
        TwoDigits <$> arbitrary <*> arbitrary
      ]

instance ToIsoparsec Digits String m

spec :: Spec
spec = do
  let parser = toIsoparsec @_ @String
  it "deserializes" $ do
    runMegaparsecParser "12" parser `shouldBe` Right (TwoDigits (SingleDigit "1") (SingleDigit "2"))
    runMegaparsecParser "125" parser `shouldBe` Right (ThreeDigits (SingleDigit "1") (SingleDigit "2") (SingleDigit "5"))
    runMegaparsecParser "1253" parser `shouldBe` Right (FourDigits (SingleDigit "1") (SingleDigit "2") (SingleDigit "5") (SingleDigit "3"))
    runMegaparsecParser "12538" parser `shouldSatisfy` isLeft
    runMegaparsecParser "2" parser `shouldSatisfy` isLeft
    runMegaparsecParser "a" parser `shouldSatisfy` isLeft
    runMegaparsecParser "1a" parser `shouldSatisfy` isLeft

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \x ->
  let s = fromJust $ runMonoidPrinter @String parser x
   in counterexample s $ case runMegaparsecParser s parser of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
  where
    parser :: Isoparsec m String => m () Digits
    parser = toIsoparsec
