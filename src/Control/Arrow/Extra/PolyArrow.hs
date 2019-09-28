{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MonoLocalBinds #-}

module Control.Arrow.Extra.PolyArrow
  ( PolyArrow(..)
  ) where

import qualified Control.Arrow                 as A
import           Control.Arrow.Extra.BaseArrow

class BaseArrow a => PolyArrow a p where
  arr :: p b c -> a b c

instance A.Arrow a => PolyArrow a (->) where
  arr = A.arr
