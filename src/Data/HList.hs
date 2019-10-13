{-# LANGUAGE PolyKinds, DataKinds, GADTs, TypeOperators, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Data.HList
  ( HList(..)
  , Applicable(..)
  ) where

data HList (ts :: [k]) where
  HNil :: HList '[]
  HCons :: k -> HList ks -> HList (k ': ks)

class Applicable f l r | f -> r where
  (<.>) :: f -> HList l -> r

instance Applicable ((->) x r) '[x] r where
  f <.> (x `HCons` HNil) = f x

instance (Applicable r' xs r) => Applicable ((->) x r') (x ': xs) r where
  f <.> (x `HCons` xs) = f x <.> xs
