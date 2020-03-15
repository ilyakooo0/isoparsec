{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Printer
  ( runMonoidPrinter,
    MonoidPrinter (..),
  )
where

import Control.Cokleisli
import Control.Monad.Writer.Lazy
import Data.Functor
import Data.Isoparsec
import Data.List.Split
import Prelude hiding ((.), id)

newtype MonoidPrinter s a b
  = MonoidPrinter {unMonoidPrinter :: Cokleisli (WriterT (Dual s) Maybe) a b}
  deriving (Category, BaseArrow, ArrowZero, ArrowPlus, ArrowChoice, PolyArrow SemiIso)

runMonoidPrinter ::
  forall s a.
  MonoidPrinter s () a ->
  a ->
  Maybe s
runMonoidPrinter p = fmap getDual . execWriterT . runCokleisli (unMonoidPrinter p)

instance IsoparsecFail (MonoidPrinter s) e where
  failure _ = MonoidPrinter . Cokleisli . const $ WriterT Nothing

instance
  (Monoid s, Eq (Element s), IsSequence s) =>
  Isoparsec (MonoidPrinter s) s
  where
  anyToken = MonoidPrinter . Cokleisli $ tell . Dual . singleton

  token t = MonoidPrinter . Cokleisli $ const $ tell . Dual . singleton $ t

  token' = MonoidPrinter . Cokleisli $ \t -> (tell . Dual . singleton) t $> t

  tokens ts = MonoidPrinter . Cokleisli $ const $ tell . Dual . fromList $ ts

  tokens' = MonoidPrinter . Cokleisli $ \ts -> (tell . Dual . fromList) ts $> ts

  chunk c = MonoidPrinter . Cokleisli . const $ tell . Dual $ c

  chunk' = MonoidPrinter . Cokleisli $ \c -> (tell . Dual) c $> c

  notToken nt = MonoidPrinter . Cokleisli $ \t ->
    if t == nt
      then empty
      else (tell . Dual . singleton) t

  tokenWhere f = MonoidPrinter . Cokleisli $ \t ->
    if f t
      then (tell . Dual . singleton) t
      else empty

  manyTokens = MonoidPrinter . Cokleisli $ \w -> do
    tell $ Dual w
    return . fromIntegral . olength $ w

  takeUntil end = MonoidPrinter . Cokleisli $ \s ->
    if length ((split . onSublist $ otoList end) . otoList $ s) > 1
      then empty
      else do
        tell . Dual $ s
        tell . Dual $ end

  tokensWhile f = MonoidPrinter . Cokleisli $ \s ->
    if oall f s
      then (tell . Dual) s
      else empty

  tokensWhile1 f = MonoidPrinter . Cokleisli $ \s ->
    if oall f s && not (onull s)
      then (tell . Dual) s
      else empty

  tuck (MonoidPrinter (Cokleisli f)) =
    MonoidPrinter . Cokleisli $ lift . fmap getDual . execWriterT . f

  tuck' (MonoidPrinter (Cokleisli f)) =
    MonoidPrinter . Cokleisli $ lift . (fmap . fmap) getDual . runWriterT . f
