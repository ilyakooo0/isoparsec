module Data.Isoparsec.Debugger.UI
  ( debug,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad
import Control.Monad.IO.Class
import Data.Isoparsec.Debugger.Internal
import Graphics.Vty
import Text.Wrap

debug :: DebuggerArrow s a b -> IO ()
debug (DebuggerArrow debugger) = do
  putStrLn . take 200 . show . rebalanceState $ debugger
  void $
    defaultMain
      app
      DebuggerState
        { displayedState = startDisplaying . rebalanceState $ debugger
        }

app :: App DebuggerState DebuggerEvent DebuggerName
app =
  App
    { appDraw = pure . renderState This . Rendered . displayedState,
      appChooseCursor = showFirstCursor,
      appHandleEvent = eventHandler,
      appStartEvent = \s -> do
        vty <- getVtyHandle
        let output = outputIface vty
        when (supportsMode output Mouse)
          $ liftIO
          $ setMode output Mouse True
        return s,
      appAttrMap = const $ attrMap defAttr []
    }

eventHandler :: DebuggerState -> BrickEvent DebuggerName DebuggerEvent -> EventM DebuggerName (Next DebuggerState)
eventHandler s (VtyEvent (EvKey KEsc _)) = halt s
eventHandler s (MouseUp n _ _) =
  continue $ s {displayedState = unRender $ expand n (Rendered $ displayedState s)}
eventHandler s _ = continue s

data DebuggerState
  = DebuggerState
      { displayedState :: DisplayDebuggerState
      }

data DisplayDebuggerState
  = DDSeq (DebugRenderState DisplayDebuggerState) (DebugRenderState DisplayDebuggerState)
  | DDAlt (DebugRenderState DisplayDebuggerState) (DebugRenderState DisplayDebuggerState)
  | DDAction DebuggerAction
  | DDEmpty
  deriving (Show)

data DebugRenderState a
  = Rendered a
  | Delayed a
  deriving (Show, Functor)

unRender :: DebugRenderState a -> a
unRender (Rendered a) = a
unRender (Delayed a) = a

startDisplaying :: IsoparsecDebugger -> DisplayDebuggerState
startDisplaying DEmpty = DDEmpty
startDisplaying (DAction a) = DDAction a
startDisplaying (DSeq a b) =
  DDSeq (Rendered $ startDisplaying a) (Delayed $ startDisplaying b)
startDisplaying (DAlt a b) =
  DDAlt (Rendered $ startDisplaying a) (Delayed $ startDisplaying b)

renderState :: DebuggerName -> DebugRenderState DisplayDebuggerState -> Widget DebuggerName
renderState n (Delayed _) = clickable n $ renderSmallNode " + "
renderState _ (Rendered DDEmpty) = renderSmallNode "*"
renderState _ (Rendered (DDAction a)) = renderNode $ show a
renderState n (Rendered (DDSeq a b)) = renderState (L n) a <=> renderState (R n) b
renderState n (Rendered (DDAlt a b)) = renderState (L n) a <+> renderState (R n) b

rebalanceState :: IsoparsecDebugger -> IsoparsecDebugger
rebalanceState DEmpty = DEmpty
rebalanceState a@(DAction _) = a
rebalanceState (DAlt (DAlt a b) c) = rebalanceState $ DAlt (rebalanceState a) (rebalanceState $ DAlt b c)
rebalanceState (DAlt a b) = DAlt (rebalanceState a) (rebalanceState b)
rebalanceState (DSeq (DSeq a b) c) = rebalanceState $ DSeq (rebalanceState a) (rebalanceState $ DSeq b c)
rebalanceState (DSeq DEmpty c) = rebalanceState c
rebalanceState (DSeq a b) = DSeq (rebalanceState a) (rebalanceState b)

expand ::
  DebuggerName ->
  DebugRenderState DisplayDebuggerState ->
  DebugRenderState DisplayDebuggerState
expand This (Delayed s) = Rendered s
expand (L n) (Rendered (DDSeq l r)) = Rendered $ DDSeq (expand n l) r
expand (L n) (Rendered (DDAlt l r)) = Rendered $ DDAlt (expand n l) r
expand (R n) (Rendered (DDAlt l r)) = Rendered $ DDAlt l (expand n r)
expand (R n) (Rendered (DDSeq l r)) = Rendered $ DDSeq l (expand n r)
expand n (Delayed s) = expand n (Rendered s)
expand _ s = s

data DebuggerName
  = This
  | L DebuggerName
  | R DebuggerName
  deriving (Eq, Ord, Show)

data DebuggerEvent = U
  deriving (Show)

nodeWidth :: Int
nodeWidth = 16

renderSmallNode :: String -> Widget n
renderSmallNode = hLimit nodeWidth . hCenter . border . str

renderNode :: String -> Widget n
renderNode = hLimit nodeWidth . hCenter . border . strWrapWith (WrapSettings False True)
