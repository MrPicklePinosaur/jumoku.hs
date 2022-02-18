module Render (runJumokuApp) where

import Brick
import Graphics.Vty
import qualified ZTree

type ResourceName = String

newtype State = State
    { ztree              :: ZTree.ZTree Integer
    } deriving (Show)

jumokuApp :: App State e ResourceName
jumokuApp = App
    { appDraw         = jumokuDraw
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = jumokuHandleEvent
    , appStartEvent   = pure
    , appAttrMap      = const $ attrMap mempty []
    }

jumokuDraw :: State -> [Widget ResourceName]
jumokuDraw s = [ vBox [ str val ] ]
    where val = maybe "E" show $ ZTree.getValue $ ztree s

jumokuHandleEvent :: State -> BrickEvent n e -> EventM n (Next State)
jumokuHandleEvent s (VtyEvent (EvKey (KChar c) [])) = case c of
    'k' -> continue s { ztree = ZTree.goUp $ ztree s }
    'h' -> continue s { ztree = ZTree.goLeft $ ztree s }
    'l' -> continue s { ztree = ZTree.goRight $ ztree s }
    'q' -> halt s
    _   -> continue s
jumokuHandleEvent s _ = continue s

jumokuInitialState :: IO State
jumokuInitialState = pure State
    { ztree = ZTree.starterZTree
    }

runJumokuApp :: IO ()
runJumokuApp = do
    initalState <- jumokuInitialState
    endState <- defaultMain jumokuApp initalState
    print endState

