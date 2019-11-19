{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Megaparsec
  ( runMegaparsec,
  )
where

import Control.Arrow.Extra
import Control.Arrow.Extra.Orphans ()
import Data.Functor
import Data.Isoparsec.Internal
import Data.Isoparsec.Tokenable
import qualified Data.Set as S
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
  (MonadParsec e s m, M.Token s ~ Token s, Tokenable s) =>
  Isoparsec (Kleisli m) s
  where

  anyToken = Kleisli $ \() -> anySingle

  token t = Kleisli $ \() -> M.single t $> ()

instance MonadParsec e s m => IsoparsecFail (Kleisli m) e where
  fail e = Kleisli $ \_ -> fancyFailure . S.singleton . ErrorCustom $ e

instance MonadParsec e s m => IsoparsecLabel (Kleisli m) String where
  label s (Kleisli m) = Kleisli $ \x -> M.label s (m x)
