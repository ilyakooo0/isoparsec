module Control.Arrow.Trans.Reader
  ( ReaderArrow (..),
    runReaderArrow,
    module Control.Arrow.Trans.Class,
  )
where

import Control.Arrow.Extra
import Control.Arrow.Trans.Class
import Control.SemiIso
import Prelude hiding ((.), fail, id)

newtype ReaderArrow r s a b
  = ReaderArrow {unReaderArrow :: s (r, a) (r, b)}

instance BaseArrow a => Category (ReaderArrow r a) where
  id = ReaderArrow . second $ id
  (ReaderArrow lhs) . (ReaderArrow rhs) = ReaderArrow $ lhs . rhs

instance (PolyArrow p a, PolyArrow SemiIso a, Eq r) => PolyArrow p (ReaderArrow r a) where
  arr = ReaderArrow . second . arr

instance (BaseArrow a, PolyArrow SemiIso a, Eq r) => BaseArrow (ReaderArrow r a) where
  (ReaderArrow lhs) *** (ReaderArrow rhs) =
    ReaderArrow $
      siDouble *** id
        ^>^ turn siTranspose2
        >>> lhs *** rhs
        >>^ siTranspose2
        >>^ turn siDouble *** id
  (ReaderArrow lhs) &&& (ReaderArrow rhs) =
    ReaderArrow $
      lhs &&& rhs
        >>^ siTranspose2
        >>^ turn siDouble *** id

instance (ArrowZero a, BaseArrow (ReaderArrow r a)) => ArrowZero (ReaderArrow r a) where
  zeroArrow = ReaderArrow $ id *** zeroArrow

instance (ArrowPlus a, ArrowZero (ReaderArrow r a)) => ArrowPlus (ReaderArrow r a) where
  (ReaderArrow lhs) <+> (ReaderArrow rhs) = ReaderArrow $ lhs <+> rhs

instance (ArrowChoice a, PolyArrow SemiIso a, Eq r) => ArrowChoice (ReaderArrow r a) where
  (ReaderArrow lhs) ||| (ReaderArrow rhs) = ReaderArrow $ siSequenceEither ^>> lhs ||| rhs
  (ReaderArrow lhs) +++ (ReaderArrow rhs) =
    ReaderArrow $ siSequenceEither ^>> lhs +++ rhs >>^ turn siSequenceEither

runReaderArrow :: (PolyArrow SemiIso s, Eq r) => r -> ReaderArrow r s a b -> s a b
runReaderArrow r a =
  turn siSnd >>> first (konst r) ^>> unReaderArrow a >>^ (first . turn $ konst r) >>^ siSnd

instance ArrowTrans (ReaderArrow r) where
  raise = ReaderArrow . second
