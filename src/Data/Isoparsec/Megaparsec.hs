{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Megaparsec
  ( runMegaparsec,
  )
where

import Control.Arrow.Extra
import Control.Arrow.Extra.Orphans ()
import Control.Monad
import Data.Functor
import Data.Isoparsec.Internal as I
import Data.Isoparsec.Tokenable
import Optics.Getter
import Optics.ReadOnly
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M
import Prelude hiding ((.))

runMegaparsec ::
  (Ord e, Stream s) =>
  Kleisli (Parsec e s) () r ->
  s ->
  Either (ParseErrorBundle s e) r
runMegaparsec (Kleisli f) = runParser (f () <* eof) ""

instance (MonadParsec e s m) => IsoparsecTry (Kleisli m) where
  try (Kleisli f) = Kleisli $ \a -> M.try (f a)

instance (MonadParsec e s m) => PolyArrow (Kleisli m) SemiIso' where
  arr (SemiIso' si) = Kleisli $ \t -> case view (getting si) t of
    Just x -> return x
    Nothing -> failure Nothing mempty

instance
  (MonadParsec e s m, M.Token s ~ Token s, s ~ M.Tokens s, Tokenable s) =>
  Isoparsec (Kleisli m) s
  where

  anyToken = Kleisli $ const anySingle

  token t = Kleisli . const $ M.single t $> ()

  manyTokens = Kleisli $ takeP Nothing . fromIntegral

  tuck (Kleisli f) = Kleisli $ \sub -> do
    sup <- getInput
    setInput sub
    r <- f () <* eof
    setInput sup
    return r

instance MonadParsec e s m => IsoparsecFail (Kleisli m) e where
  fail e = Kleisli $ \_ -> customFailure e

instance MonadParsec e s m => IsoparsecLabel (Kleisli m) String where
  label s (Kleisli m) = Kleisli $ \x -> M.label s (m x)
