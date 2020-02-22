module Control.Arrow.Reader
  ( runReaderArrow,
    ArrowReader (..),
    asksr,
    asksl,
  )
where

import Control.Arrow.Extra
import Control.SemiIso
import Prelude hiding ((.), id)

newtype ReaderArrow r s a b = ReaderArrow {unReaderArrow :: s (r, a) (r, b)}

class ArrowReader r a where
  askr :: a () r
  askl :: a r ()

asksr :: (PolyArrow SemiIso a, ArrowReader r a) => SemiIso r s -> a () s
asksr si = askr >>^ si

asksl :: (PolyArrow SemiIso a, ArrowReader r a) => SemiIso r s -> a s ()
asksl si = turn si ^>> askl

instance (PolyArrow SemiIso a, Eq r) => ArrowReader r (ReaderArrow r a) where
  askr = ReaderArrow . arr $ siFst >>> siDouble
  askl = ReaderArrow . arr $ turn siDouble >>> turn siFst

runReaderArrow :: (PolyArrow SemiIso s, Eq r) => r -> ReaderArrow r s a b -> s a b
runReaderArrow r a =
  turn siSnd >>> first (konst r) ^>> unReaderArrow a >>^ (first . turn $ konst r) >>^ siSnd
