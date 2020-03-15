{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Isoparsec.Megaparsec
  ( runMegaparsecParser,
    ParseErrorBundle (..),
    MegaparsecParser (..),
  )
where

import Control.Arrow.Extra
import Control.Arrow.Extra.Orphans ()
import Control.Monad
import Data.Functor
import Data.Functor.Identity
import Data.Isoparsec
import Data.Void
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as M
import Prelude hiding ((.))

newtype MegaparsecParser s a b
  = MegaparsecParser {unMegaparsecParser :: Kleisli (ParsecT Void s Identity) a b}
  deriving (Category, BaseArrow, ArrowZero, ArrowChoice, PolyArrow SemiIso)

instance
  (Stream s, s ~ M.Tokens s, Element s ~ M.Token s) =>
  ArrowPlus (MegaparsecParser s)
  where
  (MegaparsecParser (Kleisli lhs)) <+> (MegaparsecParser (Kleisli rhs)) =
    MegaparsecParser . Kleisli $ \x -> try (lhs x) <|> rhs x

runMegaparsecParser ::
  (Stream s) =>
  s ->
  MegaparsecParser s () r ->
  Either (ParseErrorBundle s Void) r
runMegaparsecParser s (MegaparsecParser (Kleisli f)) = runParser (f () <* eof) "" s

instance (MonadParsec e s m) => PolyArrow SemiIso (Kleisli m) where
  arr si = Kleisli $ \t -> case embed si t of
    Just x -> return x
    Nothing -> M.failure Nothing mempty

instance
  (Stream s, M.Token s ~ Element s, s ~ M.Tokens s, IsSequence s) =>
  Isoparsec (MegaparsecParser s) s
  where
  anyToken = MegaparsecParser . Kleisli $ const anySingle

  token t = MegaparsecParser . Kleisli . const $ M.single t $> ()

  token' = MegaparsecParser . Kleisli $ \t -> M.single t $> t

  tokens ts = MegaparsecParser . Kleisli . const $ M.chunk (fromList ts) $> ()

  tokens' = MegaparsecParser . Kleisli $ \ts -> M.chunk (fromList ts) $> ts

  chunk c = MegaparsecParser . Kleisli . const $ M.chunk c $> ()

  chunk' = MegaparsecParser . Kleisli $ \c -> M.chunk c $> c

  notToken t = MegaparsecParser . Kleisli . const $ M.anySingleBut t

  tokenWhere f = MegaparsecParser . Kleisli . const $ satisfy f

  manyTokens = MegaparsecParser . Kleisli $ takeP Nothing . fromIntegral

  takeUntil c = MegaparsecParser . Kleisli $ \() -> do
    ta <- M.manyTill M.anySingle (M.chunk c)
    return $ fromList ta

  tokensWhile f = MegaparsecParser . Kleisli . const $ M.takeWhileP Nothing f

  tokensWhile1 f = MegaparsecParser . Kleisli . const $ M.takeWhile1P Nothing f

  tuck (MegaparsecParser (Kleisli f)) = MegaparsecParser . Kleisli $ \sub -> do
    sup <- getInput
    setInput sub
    r <- f () <* eof
    setInput sup
    return r

  tuck' (MegaparsecParser (Kleisli f)) = MegaparsecParser . Kleisli $ \(x, sub) -> do
    sup <- getInput
    setInput sub
    r <- f x <* eof
    setInput sup
    return r
