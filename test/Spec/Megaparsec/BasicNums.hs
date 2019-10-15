{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Megaparsec.BasicNums
  ( spec,
  )
where

import Data.Either
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Optics
import Test.Hspec

data Foo = Foo Integer Integer
  deriving (Show, Eq)

makePrisms ''Foo

parser :: Isoparsec m '[Char] '[String] => m () Foo
parser = _Foo <.> number &&& whiteSpace1 &&& number

spec :: Spec
spec =
  it "deserializes" $ do
    runMegaparsec @() parser "12 31" `shouldBe` (Right $ Foo 12 31)
    runMegaparsec @() parser "1   33" `shouldBe` (Right $ Foo 1 33)
    runMegaparsec @() parser "1562" `shouldSatisfy` isLeft
