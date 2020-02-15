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

space :: (Isoparsec m s, Element s ~ Char) => m () ()
space = token ' '

unsafeWhiteSpace :: (Isoparsec m s, Element s ~ Char, Eq s) => m () ()
unsafeWhiteSpace = tokensWhile isSpace >>^ (maskr . turn . konst $ mempty)

unsafeWhiteSpace1 :: (Isoparsec m s, Element s ~ Char, Eq s) => m () ()
unsafeWhiteSpace1 = tokensWhile1 isSpace >>^ (maskr . turn . konst $ singleton ' ')

number :: (Isoparsec m s, Element s ~ Char) => m () Integer
number =
  tokensWhile1 (\c -> isNumber c || c == '+' || c == '-')
    >>^ siMaybe (readMaybe @Integer . otoList) (Just . fromList . show)
