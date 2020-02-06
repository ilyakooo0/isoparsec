module Control.SemiIso
  ( SemiIso (..),
    pattern SI,
    turn,
  )
where

import Control.Applicative
import Control.Arrow.Extra.ArrowChoice
import Control.Arrow.Extra.ArrowPlus
import Control.Arrow.Extra.ArrowZero
import Control.Arrow.Extra.BaseArrow
import Control.Category (Category (..))
import Control.Monad
import Data.Bitraversable
import Prelude hiding ((.), id)

data SemiIso a b
  = SemiIso
      { embed :: forall f. (Alternative f, Monad f) => a -> f b,
        project :: forall f. (Alternative f, Monad f) => b -> f a
      }

pattern SI ::
  forall a b.
  (forall f. (Alternative f, Monad f) => a -> f b) ->
  (forall f. (Alternative f, Monad f) => b -> f a) ->
  SemiIso a b
pattern SI a b = SemiIso a b

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
