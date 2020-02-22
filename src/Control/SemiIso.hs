module Control.SemiIso
  ( module X,
    SemiIso (..),
    pattern SI,
    siMaybe,
    siPure,
    turn,
    withSemiIso,
    AlternativeMonad,
    maskr,
    maskl,
    isoCheck,
    siCheck,
    check,
    konst,
    isoConst,
    assoc,
    ($$$),
    (*>>),
    (&>>),
    (**>),
    (>>*),
    (>>&),
    (>**),
    (<<*),
    (<<&),
    (<**),
    (*<<),
    (&<<),
    (**<),
    siFst,
    siSnd,
    siDouble,
    siSwap,
  )
where

import Control.Applicative as X
import Control.Arrow.Extra
import Control.Monad as X
import Data.Bitraversable
import Data.Tuple
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

maskr :: SemiIso a () -> SemiIso a ()
maskr (SemiIso e p) = SemiIso (\a -> e a <|> pure ()) p

maskl :: SemiIso () b -> SemiIso () b
maskl (SemiIso e p) = SemiIso e (\b -> p b <|> pure ())

infixl 1 *>>, **>, >>*, >**, <<*, <**, *<<, **<

infixl 3 $$$

($$$) :: (PolyArrow SemiIso m, Eq b) => m a b -> m c b -> m (a, c) b
a $$$ c = a *** c >>^ turn siDouble

(*>>) :: (PolyArrow SemiIso m) => m a () -> m a b -> m a b
a *>> b = a &&& b >>^ siSnd

(&>>) :: (PolyArrow SemiIso m) => m a () -> m b c -> m (a, b) c
a &>> b = a *** b >>^ siSnd

(**>) :: (PolyArrow SemiIso m) => m () () -> m a b -> m a b
a **> b = turn siSnd ^>> a *** b >>^ siSnd

(>>*) :: (PolyArrow SemiIso m, Eq b) => m a b -> m () b -> m a b
a >>* b = turn siFst ^>> a $$$ b

(>>&) :: (PolyArrow SemiIso m) => m a b -> m () c -> m a (b, c)
a >>& b = turn siFst ^>> a *** b

(>**) :: (PolyArrow SemiIso m) => m a b -> m () () -> m a b
a >** b = turn siFst ^>> a *** b >>^ siFst

(<<*) :: (PolyArrow SemiIso m, Eq b) => m a b -> m () b -> m a b
a <<* b = turn siSnd ^>> b $$$ a

(<<&) :: (PolyArrow SemiIso m) => m a b -> m () c -> m a (b, c)
a <<& b = turn siSnd ^>> b *** a >>^ siSwap

(<**) :: (PolyArrow SemiIso m) => m a b -> m () () -> m a b
a <** b = turn siSnd ^>> b *** a >>^ siSnd

(*<<) :: (PolyArrow SemiIso m) => m a () -> m a b -> m a b
a *<< b = b &&& a >>^ siFst

(&<<) :: (PolyArrow SemiIso m) => m a () -> m b c -> m (a, b) c
a &<< b = siSwap ^>> b *** a >>^ siFst

(**<) :: (PolyArrow SemiIso m) => m () () -> m a b -> m a b
a **< b = turn siFst ^>> b *** a >>^ siFst

check :: (s -> Bool) -> SemiIso s s
check f = isoCheck f id id

siCheck ::
  (s -> Bool) ->
  (forall f. AlternativeMonad f => s -> f a) ->
  (forall f. AlternativeMonad f => a -> f s) ->
  SemiIso s a
siCheck f a b =
  SI
    (\c -> guard (f c) >> a c)
    (b >=> (\c -> guard (f c) >> pure c))

isoCheck :: (s -> Bool) -> (s -> a) -> (a -> s) -> SemiIso s a
isoCheck f a b = siCheck f (pure . a) (pure . b)

isoConst :: s -> a -> SemiIso s a
isoConst s a = SI (const $ pure a) (const $ pure s)

konst :: Eq x => x -> SemiIso () x
konst x = isoConst () x >>> check (== x)

assoc :: SemiIso (a, (b, c)) ((a, b), c)
assoc = siPure (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

siFst :: SemiIso (a, ()) a
siFst = siPure fst (,())

siSnd :: SemiIso ((), a) a
siSnd = siPure snd ((),)

siDouble :: (Eq a) => SemiIso a (a, a)
siDouble = siPure (\x -> (x, x)) fst >>> check (uncurry (==))

siSwap :: SemiIso (a, b) (b, a)
siSwap = siPure swap swap
