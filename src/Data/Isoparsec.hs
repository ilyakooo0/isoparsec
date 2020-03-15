{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Isoparsec
  ( module X,
    (<.>),
    repeating,
    sepBy,
    sepBy1,
    opt,
    opt',
    morphed,
    fMorphed,
    coercing,
    mapIso,
    auto,
    specific,
    throughIntegral,
    (~>),
    (~>^),
    (^~>),
    (^~>^),
    (~|),
    (~$>),
    (~&),
    (~*),
    siPrism,
    destructHList,
    makeHListable,
    Listed,
    hmap,
  )
where

import Control.Arrow.Extra as X
import Control.Prism
import Control.Tuple.Morph
import Data.Coerce
import Data.Either
import Data.Isoparsec.Internal as X
import Data.Isoparsec.ToIsoparsec as X
import qualified Data.Map as M
import Prelude hiding ((.), fail, id)

opt :: (ArrowPlus m, PolyArrow SemiIso m) => m () () -> m () ()
opt m = m <+^ konst ()

opt' :: (ArrowPlus m, PolyArrow SemiIso m, Eq a) => a -> m () a -> m () ()
opt' a m = opt (m >>^ turn (konst a))

repeating :: (PolyArrow SemiIso m, ArrowPlus m, Eq b) => m () b -> m () [b]
repeating m = (m &&& (repeating m <+^ konst [])) >>^ siCons

sepBy :: (PolyArrow SemiIso m, ArrowPlus m, Eq a) => m () () -> m () a -> m () [a]
sepBy sep a = sepBy1 sep a <+^ konst []

sepBy1 :: (PolyArrow SemiIso m, ArrowPlus m, Eq a) => m () () -> m () a -> m () [a]
sepBy1 sep a = (a &&& repeating (sep *>> a) <+^ konst []) >>^ siCons

infixl 0 <.>

(<.>) ::
  (PolyArrow SemiIso m, TupleMorphable x c, TupleMorphable a c) =>
  (Prism' a b) ->
  m x' x ->
  m x' b
b <.> p = (p >>^ morphed) >>^ siPrism b

siPrism ::
  (Prism' a b) ->
  SemiIso a b
siPrism p = withPrism p $ \x y -> SemiIso (pure . x) (either (const empty) pure . y)

type Listed m a b = m (HList (TupleHListContents a)) (HList (TupleHListContents b))

infixl 8 ~>, ~>^, ^~>, ^~>^

type family If cond t f where
  If 'True t _ = t
  If 'False _ f = f

type family BothEmpty a b where
  BothEmpty '[] '[] = 'True
  BothEmpty _ _ = 'False

class BothEmpty c' d' ~ flag => SmartCompose b' c' d' flag | c' d' -> flag where
  (~>) ::
    ( PolyArrow SemiIso m,
      FlatMorphable a a' fa,
      FlatMorphable b b' fb,
      FlatMorphable c c' fc,
      FlatMorphable d d' fd
    ) =>
    m a b ->
    m c d ->
    m (HList a') (HList (If flag b' d'))

instance SmartCompose b '[] '[] 'True where
  (~>) lhs rhs = turn siHFst ^>> (makeHListable lhs *** makeHListable rhs) >>^ siHFst

instance SmartCompose (c ': cc) (c ': cc) d 'False where
  (~>) lhs rhs = makeHListable lhs >>> makeHListable rhs

instance SmartCompose '[] '[] (d ': dd) 'False where
  (~>) lhs rhs = makeHListable lhs >>> makeHListable rhs

siHFst :: SemiIso (a, HList '[]) a
siHFst = siPure fst (,HNil)

(~>^) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a a' fa,
    FlatMorphable b bc fb,
    FlatMorphable c bc 'False,
    FlatMorphable d d' 'False
  ) =>
  m a b ->
  SemiIso c d ->
  m (HList a') (HList d')
lhs ~>^ rhs = makeHListable lhs >>> makeHListable (arr rhs)

(^~>) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a a' 'False,
    FlatMorphable b bc 'False,
    FlatMorphable c bc fc,
    FlatMorphable d d' fd
  ) =>
  SemiIso a b ->
  m c d ->
  m (HList a') (HList d')
lhs ^~> rhs = makeHListable (arr lhs) >>> makeHListable rhs

(^~>^) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a a' 'False,
    FlatMorphable b b' 'False,
    FlatMorphable d d' 'False
  ) =>
  SemiIso a b ->
  SemiIso b d ->
  m (HList a') (HList d')
lhs ^~>^ rhs = makeHListable . arr $ lhs >>> rhs

infixl 5 ~|

(~|) ::
  ( PolyArrow SemiIso m,
    ArrowPlus m,
    FlatMorphable a ac fa,
    FlatMorphable c ac fb,
    FlatMorphable b bd fc,
    FlatMorphable d bd fd
  ) =>
  m a b ->
  m c d ->
  m (HList ac) (HList bd)
lhs ~| rhs = makeHListable lhs <+> makeHListable rhs

infixl 7 ~&

(~&) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a ac' fa,
    FlatMorphable c ac' fc,
    FlatMorphable b b' fb,
    FlatMorphable d d' fd,
    AppendableList b' d' f
  ) =>
  m a b ->
  m c d ->
  m (HList ac') (HList f)
lhs ~& rhs = (makeHListable lhs &&& makeHListable rhs) >>^ consHList

infixl 9 ~*

(~*) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a a' fa,
    FlatMorphable c c' fc,
    FlatMorphable b b' fb,
    FlatMorphable d d' fd,
    AppendableList a' c' e',
    AppendableList b' d' f'
  ) =>
  m a b ->
  m c d ->
  m (HList e') (HList f')
lhs ~* rhs = turn consHList ^>> (makeHListable lhs *** makeHListable rhs) >>^ consHList

infixl 6 ~$>

(~$>) ::
  ( PolyArrow SemiIso m,
    FlatMorphable a a' fa,
    FlatMorphable b bc fb,
    FlatMorphable c bc fc,
    FlatMorphable d d' fd
  ) =>
  m a b ->
  Prism' c d ->
  m (HList a') (HList d')
lhs ~$> rhs = makeHListable lhs >>> makeHListable (arr (siPrism rhs))

hmap ::
  ( FlatMorphable b b' fb,
    FlatMorphable a a' fa,
    FlatMorphable d d' fd,
    FlatMorphable c c' fc,
    PolyArrow SemiIso m
  ) =>
  (m a b -> m c d) ->
  m (HList a') (HList b') ->
  m (HList c') (HList d')
hmap f m = turn fMorphed ^>> (f $ fMorphed ^>> m >>^ turn fMorphed) >>^ fMorphed

coercing :: forall b a. Coercible a b => SemiIso a b
coercing = siPure coerce coerce

morphed :: (TupleMorphable a c, TupleMorphable b c) => SemiIso a b
morphed = siPure morphTuples morphTuples

fMorphed :: (FlatMorphable a b fa) => SemiIso a (HList b)
fMorphed = siPure flatUnmorph flatMorph

makeHListable ::
  (FlatMorphable a a' fa, FlatMorphable b b' fb, PolyArrow SemiIso m) =>
  m a b ->
  m (HList a') (HList b')
makeHListable m = turn fMorphed ^>> m >>^ fMorphed

destructHList ::
  ( FlatMorphable a a' 'False,
    FlatMorphable b b' 'False,
    PolyArrow SemiIso m,
    HListContentsTuple a' ~ a,
    HListContentsTuple b' ~ b,
    TupleHListContents a ~ a',
    TupleHListContents b ~ b'
  ) =>
  m (HList a') (HList b') ->
  m a b
destructHList m = fMorphed ^>> m >>^ turn fMorphed

consHList ::
  forall a b c.
  AppendableList a b c =>
  SemiIso (HList a, HList b) (HList c)
consHList =
  siPure
    (uncurry (++:))
    (\c -> (hTake @(Length a) Proxy c, hDrop @(Length a) Proxy c))

mapIso :: (PolyArrow SemiIso m, Ord a, Ord b) => [(a, b)] -> m a b
mapIso m = arr $ siMaybe (`M.lookup` n) (`M.lookup` u)
  where
    n = M.fromListWith (error "mapping not unique") m
    u =
      M.fromListWith (error "mapping not unique") $
        (\(a, b) -> (b, a)) <$> m

auto :: forall x s m. (ToIsoparsec x s m, Isoparsec m s) => m () x
auto = toIsoparsec

specific :: forall x s m. (ToIsoparsec x s m, Isoparsec m s, Eq x) => x -> m () ()
specific x = auto @x >>^ turn (konst x)

throughIntegral ::
  (Integral a, Integral b, Num a, Num b) =>
  SemiIso a b
throughIntegral = siPure fromIntegral fromIntegral
