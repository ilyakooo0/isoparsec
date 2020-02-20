{-# LANGUAGE TemplateHaskell #-}

module Spec.BasicArrowFree
  (
  )
where

import Control.Arrow.Extra
import Control.Arrow.Free
import Control.Lens.TH
import Control.Monad.Reader
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Test.Hspec
import Test.Tasty
import Prelude hiding ((.), id)

data ConstSep = ConstSep Integer Integer
  deriving (Eq, Show)

makePrisms ''ConstSep

instance FreeArrow (MonadReader String) a => ToIsoparsec ConstSep String a where
  toIsoparsec = _ConstSep <.> (number >** bifree @(MonadReader String) ask chunk') &&& number

spec :: Spec
spec = do
  let run = flip runReader (" " :: String) . runArrowFree () . runMegaparsecT @() @String (auto @ConstSep)
  it "deserializes" $ do
    run "12 31" `shouldBe` Right (ConstSep 12 31)
-- runMegaparsec @() @String (auto @ConstSep) "1   33" `shouldBe` Right (ConstSep 1 33)
