module Control.SemiIso
  ( module X,
    SemiIso (..),
    pattern SI,
    siMaybe,
    siPure,
    turn,
    withSemiIso,
    AlternativeMonad,
  )
where

import Control.Applicative as X
import Control.Arrow.Extra
import Control.Monad as X
import Data.Bitraversable
import Prelude hiding ((.), id)

type AlternativeMonad m = (Alternative m, Monad m)

data SemiIso a b
  = SemiIso
      { embed :: forall f. AlternativeMonad f => a -> f b,
        project :: forall f. AlternativeMonad f => b -> f a
      }

pattern SI ::
  forall a b.
  (forall f. AlternativeMonad f => a -> f b) ->
  (forall f. AlternativeMonad f => b -> f a) ->
  SemiIso a b
pattern SI a b = SemiIso a b

siMaybe :: (a -> Maybe b) -> (b -> Maybe a) -> SemiIso a b
siMaybe a b = SI (maybeToAlternative . a) (maybeToAlternative . b)
  where
    maybeToAlternative :: Alternative f => Maybe a -> f a
    maybeToAlternative (Just x) = pure x
    maybeToAlternative Nothing = empty

siPure :: (a -> b) -> (b -> a) -> SemiIso a b
siPure a b = SI (pure . a) (pure . b)

instance Category SemiIso where
  id = SemiIso pure pure
  (SemiIso a1 b1) . (SemiIso a2 b2) = SemiIso (a1 <=< a2) (b2 <=< b1)

instance BaseArrow SemiIso where
  (SemiIso a1 b1) *** (SemiIso a2 b2) =
    SemiIso
      (\(x, y) -> (,) <$> a1 x <*> a2 y)
      (\(x, y) -> (,) <$> b1 x <*> b2 y)
  (SemiIso a1 b1) &&& (SemiIso a2 b2) =
    SemiIso
      (\x -> (,) <$> a1 x <*> a2 x)
      (\(x, y) -> b1 x <|> b2 y)

instance ArrowChoice SemiIso where
  (SemiIso a1 b1) +++ (SemiIso a2 b2) =
    SemiIso (bitraverse a1 a2) (bitraverse b1 b2)
  (SemiIso a1 b1) ||| (SemiIso a2 b2) =
    SemiIso
      (either a1 a2)
      (\x -> (Left <$> b1 x) <|> (Right <$> b2 x))

instance ArrowZero SemiIso where
  zeroArrow = SemiIso (const empty) (const empty)

instance ArrowPlus SemiIso where
  (SemiIso a1 b1) <+> (SemiIso a2 b2) =
    SemiIso
      (\x -> a1 x <|> a2 x)
      (\x -> b1 x <|> b2 x)

-- | prop> turn . turn = id
turn :: SemiIso a b -> SemiIso b a
turn (SemiIso a b) = SemiIso b a

withSemiIso ::
  AlternativeMonad f =>
  SemiIso a b ->
  ((a -> f b) -> (b -> f a) -> c) ->
  c
withSemiIso (SemiIso a b) f = f a b
