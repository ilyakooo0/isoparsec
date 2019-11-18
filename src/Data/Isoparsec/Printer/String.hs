{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Printer.String
  (
  )
where

import Control.Monad.Writer.Lazy
import Data.Isoparsec.Cokleisli
import Data.Isoparsec.Internal

instance IsoparsecTry (Cokleisli (WriterT (Dual String) m)) where
  try = id

instance MonadPlus m => Isoparsec (Cokleisli (WriterT (Dual String) m)) String Char where

  token t = Cokleisli $ const $ tell . Dual . pure $ t

  anyToken = Cokleisli $ tell . Dual . pure
