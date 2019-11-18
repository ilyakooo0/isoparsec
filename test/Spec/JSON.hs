{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.JSON
  ( quickSpec,
  )
where

import Control.Arrow.Extra
import Data.Isoparsec
import Data.Isoparsec.Char
import Data.Isoparsec.Megaparsec
import Data.Isoparsec.Printer
import Data.Maybe
import Data.Void
import Optics hiding (elements)
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

quickSpec :: TestTree
quickSpec =
  testProperty "roundtrips" $ \x ->
    let s = fromJust $ runPrinter @Maybe @String json x
     in counterexample s $ case runMegaparsec @Void json s of
          Right y -> property $ x == y
          Left err -> counterexample (errorBundlePretty err) False

json :: (PolyArrow m SemiIso', Isoparsec m String Char) => m () JSON
json = si' Just Just ^<< (try string <+> try array <+> try integer <+> object)
  where
    string' = token '"' &&& tokensWhile (/= '"') &&& token '"'
    string = _JString <.> string'
    array =
      _Array
        <.> token '['
          &&& whiteSpace
          &&& ( try
                  ( ( ( json &&& whiteSpace
                          &&& try
                            ( repeating
                                ( (token ',' &&& whiteSpace &&& json) >>% morphed
                                )
                            )
                          <+> konst []
                      )
                        >>% morphed
                    )
                      >>^ cons'
                  )
                  <+> konst []
              )
          &&& whiteSpace
          &&& token ']'
    integer = _JInteger <.> number
    object =
      _Object <.> token '{' &&& whiteSpace
        &&& ( try
                ( let pair = (whiteSpace &&& string' &&& whiteSpace &&& token ':' &&& whiteSpace &&& json &&& whiteSpace) >>% morphed
                   in (pair &&& try (repeating ((token ',' &&& whiteSpace &&& pair) >>% morphed) <+> konst [])) >>^ cons'
                )
                <+> konst []
            )
        &&& whiteSpace
        &&& token '}'
