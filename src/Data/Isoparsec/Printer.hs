{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Printer
  ( module X,
    runPrinter,
  )
where

import Control.Monad.Writer.Lazy
import Data.Isoparsec.Cokleisli
import Data.Isoparsec.Internal
import Data.Isoparsec.Printer.String as X ()

runPrinter ::
  Monad m =>
  Cokleisli (WriterT (Dual s) m) () a ->
  a ->
  m s
runPrinter p = fmap getDual . execWriterT . unCokleisli p

instance IsoparsecFail (Cokleisli (WriterT s Maybe)) e where
  fail _ = Cokleisli . const $ WriterT Nothing

instance IsoparsecFail (Cokleisli (WriterT s (Either e))) e where
  fail = Cokleisli . const . WriterT . Left

instance IsoparsecLabel (Cokleisli (WriterT s Maybe)) e where
  label _ = id

instance IsoparsecLabel (Cokleisli (WriterT s (Either e))) y where
  label _ = id
