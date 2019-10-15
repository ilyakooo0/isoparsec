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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.Isoparsec.Internal
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Optics.Getter
import Optics.ReadOnly
import Text.Megaparsec as M
import Prelude hiding ((.))

runMegaparsec :: Kleisli (Parsec e s) () r -> s -> Either (ParseErrorBundle s e) r
runMegaparsec (Kleisli f) = runParser (f ()) ""

instance (MonadParsec e s m) => IsoparsecTry (Kleisli m) where
  try (Kleisli f) = Kleisli $ \a -> M.try (f a)

instance (MonadParsec e s m) => PolyArrow (Kleisli m) SemiIso' where
  arr (SemiIso' si) = Kleisli $ \t -> case view (getting si) t of
    Just x -> return x
    Nothing -> failure Nothing mempty

instance
  (MonadParsec e s' m, Token s' ~ s) =>
  IsoparsecBase s (Kleisli m)
  where
  anyToken = Kleisli $ \() -> anySingle

instance
  (MonadParsec e s m, Token s ~ Char) =>
  IsoparsecTokenable Char (Kleisli m)
  where
  token t = Kleisli $ \() -> single t $> ()

instance
  (MonadParsec e s m, Token s ~ Word8) =>
  IsoparsecTokenable Word8 (Kleisli m)
  where
  token t = Kleisli $ \() -> single t $> ()

instance
  (MonadParsec e s m, Tokens s ~ String) =>
  IsoparsecTokenable String (Kleisli m)
  where
  token t = Kleisli $ \() -> chunk t $> ()

instance
  (MonadParsec e s m, Tokens s ~ B.ByteString) =>
  IsoparsecTokenable B.ByteString (Kleisli m)
  where
  token t = Kleisli $ \() -> chunk t $> ()

instance
  (MonadParsec e s m, Tokens s ~ BL.ByteString) =>
  IsoparsecTokenable BL.ByteString (Kleisli m)
  where
  token t = Kleisli $ \() -> chunk t $> ()

instance
  (MonadParsec e s m, Tokens s ~ T.Text) =>
  IsoparsecTokenable T.Text (Kleisli m)
  where
  token t = Kleisli $ \() -> chunk t $> ()

instance
  (MonadParsec e s m, Tokens s ~ TL.Text) =>
  IsoparsecTokenable TL.Text (Kleisli m)
  where
  token t = Kleisli $ \() -> chunk t $> ()

instance MonadParsec e s m => IsoparsecFail (Kleisli m) e where
  fail e = Kleisli $ \_ -> fancyFailure . S.singleton . ErrorCustom $ e
