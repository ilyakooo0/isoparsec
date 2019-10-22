{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Isoparsec.Printer.String
  (
  )
where

import Control.Monad.Fail as MF
import Control.Monad.Writer.Lazy
import Data.Isoparsec.Cokleisli
import Data.Isoparsec.Internal

runStringPrinter ::
  MonadFail m =>
  Cokleisli (WriterT String m) () a ->
  a ->
  m String
runStringPrinter p a = execWriterT $ unCokleisli p a

instance MonadPlus m => IsoparsecTry (Cokleisli (WriterT (Dual String) m)) where
  try = id

instance MonadPlus m => IsoparsecBase Char (Cokleisli (WriterT (Dual String) m)) where
  anyToken = Cokleisli $ tell . Dual . pure

instance MonadPlus m => IsoparsecTokenable Char (Cokleisli (WriterT (Dual String) m)) where
  token t = Cokleisli $ const $ tell . Dual . pure $ t

instance MonadPlus m => IsoparsecTokenable String (Cokleisli (WriterT (Dual String) m)) where
  token t = Cokleisli $ const . tell . Dual $ t
