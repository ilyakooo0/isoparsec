module Data.Isoparsec.Char
  ( unsafeWhiteSpace,
    unsafeWhiteSpace1,
    space,
    number,
  )
where

import Data.Char
import Data.Isoparsec
import Text.Read
import Prelude hiding ((.), id)

space :: (Isoparsec m s, Token s ~ Char) => m () ()
space = token ' '

unsafeWhiteSpace :: (Isoparsec m s, Token s ~ Char) => m () ()
unsafeWhiteSpace = tokensWhile isSpace >>> badTsnok mempty

unsafeWhiteSpace1 :: (Isoparsec m s, Token s ~ Char) => m () ()
unsafeWhiteSpace1 = tokensWhile1 isSpace >>> badTsnok (liftToken ' ')

number :: (Isoparsec m s, Token s ~ Char) => m () Integer
number =
  tokensWhile1 (\c -> isNumber c || c == '+' || c == '-')
    >>^ siMaybe (readMaybe @Integer . lowerTokens) (Just . liftTokens . show)
