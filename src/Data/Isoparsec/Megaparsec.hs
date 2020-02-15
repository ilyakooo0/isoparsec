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
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M
import Prelude hiding ((.))

runMegaparsec ::
  (Ord e, Stream s) =>
  Kleisli (Parsec e s) () r ->
  s ->
  Either (ParseErrorBundle s e) r
runMegaparsec (Kleisli f) = runParser (f () <* eof) ""

instance (MonadParsec e s m) => PolyArrow (Kleisli m) SemiIso where
  arr si = Kleisli $ \t -> case embed si t of
    Just x -> return x
    Nothing -> M.failure Nothing mempty

instance
  (MonadParsec e s m, M.Token s ~ Element s, s ~ M.Tokens s, IsSequence s) =>
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
  failure e = Kleisli $ \_ -> customFailure e

instance (MonadParsec e s m, M.Token s ~ Element s, s ~ M.Tokens s) => ArrowPlus (Kleisli m) where
  (Kleisli lhs) <+> (Kleisli rhs) = Kleisli $ \x -> try (lhs x) <|> rhs x
