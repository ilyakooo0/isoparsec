module Data.Isoparsec.Debugger.Internal
  ( IsoparsecDebugger (..),
    DebuggerAction (..),
    DebuggerArrow (..),
  )
where

import Data.Isoparsec
import Prelude hiding ((.), id)

data IsoparsecDebugger
  = DEmpty
  | DSeq IsoparsecDebugger IsoparsecDebugger
  | DAlt IsoparsecDebugger IsoparsecDebugger
  | DAction DebuggerAction
  deriving (Show)

data DebuggerAction
  = AnyToken
  | Token String
  | Token'
  | Tokens String
  | Tokens'
  | Chunk String
  | Chunk'
  | NotToken String
  | TokenWhere
  | ManyTokens
  | TakeUntil String
  | TokensWhile
  | TokensWhile1
  | Other String
  deriving (Eq, Ord, Show)

newtype DebuggerArrow s a b = DebuggerArrow {runDebuggerArrow :: IsoparsecDebugger}

instance Category (DebuggerArrow s) where
  id = DebuggerArrow DEmpty
  (DebuggerArrow a) . (DebuggerArrow b) = DebuggerArrow $ DSeq b a

instance BaseArrow (DebuggerArrow s) where
  (DebuggerArrow a) *** (DebuggerArrow b) = DebuggerArrow $ DSeq b a
  (DebuggerArrow a) &&& (DebuggerArrow b) = DebuggerArrow $ DSeq b a
  first (DebuggerArrow a) = DebuggerArrow a
  second (DebuggerArrow a) = DebuggerArrow a

instance ArrowZero (DebuggerArrow s) where
  zeroArrow = DebuggerArrow DEmpty

instance ArrowPlus (DebuggerArrow s) where
  (DebuggerArrow a) <+> (DebuggerArrow b) = DebuggerArrow $ DAlt b a

instance ArrowChoice (DebuggerArrow s) where
  (DebuggerArrow a) ||| (DebuggerArrow b) = DebuggerArrow $ DAlt b a
  (DebuggerArrow a) +++ (DebuggerArrow b) = DebuggerArrow $ DAlt b a

instance PolyArrow p (DebuggerArrow s) where
  arr _ = DebuggerArrow DEmpty

instance (IsSequence s, Show s) => Isoparsec (DebuggerArrow s) s where
  anyToken = DebuggerArrow $ DAction AnyToken
  token = DebuggerArrow . DAction . Token . show . singleton @s
  token' = DebuggerArrow $ DAction Token'
  tokens = DebuggerArrow . DAction . Tokens . show . fromList @s
  tokens' = DebuggerArrow $ DAction Tokens'
  chunk = DebuggerArrow . DAction . Chunk . show
  chunk' = DebuggerArrow $ DAction Chunk'
  notToken = DebuggerArrow . DAction . NotToken . show . singleton @s
  tokenWhere _ = DebuggerArrow $ DAction TokenWhere
  manyTokens = DebuggerArrow $ DAction ManyTokens
  takeUntil = DebuggerArrow . DAction . TakeUntil . show
  tokensWhile _ = DebuggerArrow $ DAction TokensWhile
  tokensWhile1 _ = DebuggerArrow $ DAction TokensWhile1
  tuck (DebuggerArrow a) = DebuggerArrow a
  tuck' (DebuggerArrow a) = DebuggerArrow a
