{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.Isoparsec.Char
  ( whiteSpace,
    whiteSpace1,
    space,
    number,
  )
where

import Data.Char
import Data.Isoparsec
import qualified Data.List.NonEmpty as NE
import Text.Read
import Prelude hiding ((.), id)

space :: Isoparsec m '[Char] '[] => m () ()
space = token ' '

whiteSpace :: Isoparsec m '[Char] '[] => m () ()
whiteSpace = tokensWhile isSpace >>> tsnok []

whiteSpace1 :: Isoparsec m '[Char] '[] => m () ()
whiteSpace1 = tokensWhile1 isSpace >>> tsnok (pure ' ')

number :: Isoparsec m '[Char] '[String] => m () Integer
number =
  (try (token '+') <+> try (token '-') <+> konst ())
    >>> tokensWhile1 isNumber
    >>^ si' (readMaybe @Integer . NE.toList) (NE.nonEmpty . show)
