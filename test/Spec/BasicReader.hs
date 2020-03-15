{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.BasicReader
  ( spec,
  )
where

import Control.Arrow.Extra
import Control.Arrow.Reader
import Control.Lens.TH
import Data.Either
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Test.Hspec
import Prelude hiding ((.), id)

data ConstSep = ConstSep Integer Integer
  deriving (Eq, Show)

makePrisms ''ConstSep

instance ArrowReader String a => ToIsoparsec ConstSep String a where
  toIsoparsec = _ConstSep <.> (number >** biask chunk') &&& number

spec :: Spec
spec = do
  let run (sep :: String) (s :: String) =
        runMegaparsecParser s . runReaderArrow sep $ (auto @ConstSep)
  it "deserializes" $ do
    run "s" "12s31" `shouldBe` Right (ConstSep 12 31)
    run " " "12 31" `shouldBe` Right (ConstSep 12 31)
    run "oh no" "12oh no31" `shouldBe` Right (ConstSep 12 31)
    run "oh no" "12oh no 31" `shouldSatisfy` isLeft
    run "oh no" "12ohno31" `shouldSatisfy` isLeft
    run "&" "12 31" `shouldSatisfy` isLeft
    run "&" "12&31" `shouldBe` Right (ConstSep 12 31)
