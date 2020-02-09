module Control.Prism
  ( withPrism,
    Market,
    Identity,
  )
where

import Data.Coerce
import Data.Functor.Identity
import Data.Profunctor

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  lmap f (Market bt seta) = Market bt (seta . f)
  rmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a

type APrism b a = Market a a a (Identity a) -> Market a a b (Identity b)

withPrism :: APrism b a -> ((a -> b) -> (b -> Either b a) -> r) -> r
withPrism k f = case coerce (k (Market Identity Right)) of
  Market bt seta -> f bt seta
