{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

space :: (Isoparsec m s, Token s ~ Char) => m () ()
space = token ' '

whiteSpace :: (Isoparsec m s, Token s ~ Char) => m () ()
whiteSpace = tokensWhile isSpace >>> tsnok []

whiteSpace1 :: (Isoparsec m s, Token s ~ Char) => m () ()
whiteSpace1 = tokensWhile1 isSpace >>> tsnok (pure ' ')

number :: (Isoparsec m s, Token s ~ Char) => m () Integer
number =
  tokensWhile1 (\c -> isNumber c || c == '+' || c == '-')
    >>^ si' (readMaybe @Integer . NE.toList) (NE.nonEmpty . show)
