{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Printer
  ( runPrinter,
  )
where

import Control.Cokleisli
import Control.Monad.Writer.Lazy
import Data.Isoparsec
import Prelude hiding ((.), id)

runPrinter ::
  forall m s a.
  Monad m =>
  Cokleisli (WriterT (Dual s) m) () a ->
  a ->
  m s
runPrinter p = fmap getDual . execWriterT . runCokleisli p

instance IsoparsecFail (Cokleisli (WriterT s Maybe)) e where
  failure _ = Cokleisli . const $ WriterT Nothing

instance IsoparsecFail (Cokleisli (WriterT s (Either e))) e where
  failure = Cokleisli . const . WriterT . Left

instance
  (MonadPlus m, Monoid s, Eq (Element s), Eq s, IsSequence s, Show s) =>
  Isoparsec (Cokleisli (WriterT (Dual s) m)) s
  where
  token t = Cokleisli $ const $ tell . Dual . singleton $ t

  anyToken = Cokleisli $ tell . Dual . singleton

  manyTokens = Cokleisli $ \w -> do
    tell $ Dual w
    return . fromIntegral . olength $ w

  tuck' (Cokleisli f) = Cokleisli $ lift . (fmap . fmap) getDual . runWriterT . f
