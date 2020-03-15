{-# LANGUAGE TemplateHaskell #-}

module Spec.JSONHList
  ( quickSpec,
    spec,
  )
where

import Control.Arrow.Extra
import Control.Lens.TH
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Spec.Helper
import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Megaparsec.Error
import Prelude hiding ((.))

data JSON
  = Object [(String, JSON)]
  | JString String
  | Array [JSON]
  | JInteger Integer
  deriving (Show, Eq)

instance Arbitrary JSON where
  arbitrary =
    oneof
      [ Object <$> listOf ((,) <$> listOf (elements ['.' .. 'z']) <*> scale (`div` 2) arbitrary),
        JString <$> listOf (elements ['.' .. 'z']),
        Array <$> listOf (scale (`div` 2) arbitrary),
        JInteger <$> arbitrary
      ]

  shrink (Object a) = Object <$> shrink a
  shrink (JString a) = JString <$> shrink a
  shrink (Array a) = Array <$> shrink a
  shrink (JInteger a) = JInteger <$> shrink a

makePrisms ''JSON

spec :: Spec
spec =
  it "deserializes" $ do
    "{\"foo\": 8}" `shouldParseS` Object [("foo", JInteger 8)]
    "{\"foo\": 8, \"\": \"\"}" `shouldParseS` Object [("foo", JInteger 8), ("", JString "")]
    "{}" `shouldParseS` Object []
    "[{\"foo\": 8}, 2, 3]" `shouldParseS` Array [Object [("foo", JInteger 8)], JInteger 2, JInteger 3]
    "[{\"foo\": \"oh no\"}, 2, 3]" `shouldParseS` Array [Object [("foo", JString "oh no")], JInteger 2, JInteger 3]

quickSpec :: TestTree
quickSpec =
  testProperty "roundtrips" $ \x ->
    let s = fromJust $ runMonoidPrinter @String json x
     in counterexample s $ case runMegaparsecParser s json of
          Right y -> property $ x == y
          Left err -> counterexample (errorBundlePretty err) False

instance ToIsoparsec JSON String a where
  toIsoparsec = json

json :: forall m. Isoparsec m String => m () JSON
json = destructHList $ string ~| array ~| integer ~| object
  where
    string' :: Listed m () String
    string' = token '"' ~> tokensWhile (/= '"') ~> token '"'
    string :: Listed m () JSON
    string = string' ~$> _JString
    array :: Listed m () JSON
    array =
      token '['
        ~> unsafeWhiteSpace
        ~> sepByComma json
        ~> unsafeWhiteSpace
        ~> token ']'
        ~$> _Array
    integer = number ~$> _JInteger
    object =
      token '{' ~> unsafeWhiteSpace
        ~> hmap sepByComma pair
        ~> unsafeWhiteSpace
        ~> token '}'
        ~$> _Object
    pair =
      unsafeWhiteSpace ~> string' ~> unsafeWhiteSpace ~> token ':' ~> unsafeWhiteSpace
        ~& json ~> unsafeWhiteSpace
    sepByComma :: Eq a => m () a -> m () [a]
    sepByComma = sepBy (unsafeWhiteSpace >>> token ',' >>> unsafeWhiteSpace)
