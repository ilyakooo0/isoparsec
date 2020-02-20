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

data ConstSep = ConstSep Integer Integer

makePrisms ''ConstSep

instance FreeArrow (Reader String) a => ToIsoparsec ConstSep String a where
  toIsoparsec = ConstSep <.> number >>* (freer ask >>> chunk)
