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
  failure _ = Cokleisli . const $ WriterT Nothing

instance IsoparsecFail (Cokleisli (WriterT s (Either e))) e where
  failure = Cokleisli . const . WriterT . Left

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
