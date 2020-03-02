module Control.Arrow.Reader
  ( ArrowReader (..),
    asksr,
    asksl,
    biask,
    biasks,
    module Control.Arrow.Trans.Reader,
  )
where

import Control.Arrow.Extra
import Control.Arrow.Trans.Reader
import Control.SemiIso
import Prelude hiding ((.), id)

class ArrowReader r a where
  askr :: a () r
  askl :: a r ()

instance (PolyArrow SemiIso a, Eq r) => ArrowReader r (ReaderArrow r a) where
  askr = ReaderArrow . arr $ siFst >>> siDouble
  askl = ReaderArrow . arr $ turn siDouble >>> turn siFst

asksr :: (PolyArrow SemiIso a, ArrowReader r a) => SemiIso r s -> a () s
asksr si = askr >>^ si

asksl :: (PolyArrow SemiIso a, ArrowReader r a) => SemiIso r s -> a s ()
asksl si = turn si ^>> askl

biask :: (ArrowReader r a, Category a) => a r r -> a () ()
biask a = askr >>> a >>> askl

biasks :: (ArrowReader r a, PolyArrow SemiIso a) => SemiIso r s -> a s s -> a () ()
biasks si a = asksr si >>> a >>> asksl si
