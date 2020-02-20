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
import Data.Void
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

instance (Arbitrary SingleDigit) m where
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
    runMegaparsec @() parser "12" `shouldBe` Right (TwoDigits (SingleDigit "1") (SingleDigit "2"))
    runMegaparsec @() parser "125" `shouldBe` Right (ThreeDigits (SingleDigit "1") (SingleDigit "2") (SingleDigit "5"))
    runMegaparsec @() parser "1253" `shouldBe` Right (FourDigits (SingleDigit "1") (SingleDigit "2") (SingleDigit "5") (SingleDigit "3"))
    runMegaparsec @() parser "12538" `shouldSatisfy` isLeft
    runMegaparsec @() parser "2" `shouldSatisfy` isLeft
    runMegaparsec @() parser "a" `shouldSatisfy` isLeft
    runMegaparsec @() parser "1a" `shouldSatisfy` isLeft

quickSpec :: TestTree
quickSpec = testProperty "roundtrips" $ \x ->
  let s = fromJust $ runPrinter @Maybe @String parser x
   in counterexample s $ case runMegaparsec @Void parser s of
        Right y -> property $ x == y
        Left err -> counterexample (errorBundlePretty err) False
  where
    parser :: Isoparsec m String => m () Digits
    parser = toIsoparsec
