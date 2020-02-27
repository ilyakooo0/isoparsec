module Control.Arrow.Trans.Class
  ( ArrowTrans (..),
  )
where

import Control.Arrow.Extra

class ArrowTrans s where
  raise :: BaseArrow a => a b c -> s a b c
