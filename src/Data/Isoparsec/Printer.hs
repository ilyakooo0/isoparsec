{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Printer
  ( runPrinter,
  )
where

import Control.Monad.Writer.Lazy
import Data.Isoparsec
import Data.Isoparsec.Cokleisli
import Prelude hiding ((.), id)

runPrinter ::
  forall m s a.
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

instance IsoparsecTry (Cokleisli (WriterT (Dual s) m)) where
  try = id

instance
  (MonadPlus m, Monoid s, Eq (Token s), Tokenable s, Show s) =>
  Isoparsec (Cokleisli (WriterT (Dual s) m)) s
  where

  token t = Cokleisli $ const $ tell . Dual . liftToken $ t

  anyToken = Cokleisli $ tell . Dual . liftToken

  manyTokens = Cokleisli $ \w -> do
    tell $ Dual w
    return . fromIntegral . length . lowerTokens $ w

  tuck (Cokleisli f) = Cokleisli $ lift . fmap getDual . execWriterT . f
