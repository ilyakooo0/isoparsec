{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Orphans
  (
  )
where

import Control.Arrow.Extra
import Control.Monad.Reader

instance MonadReader r m => MonadReader r (Kleisli m ())
