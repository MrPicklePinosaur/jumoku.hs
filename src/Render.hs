module Render (runJumokuApp) where

import Brick
import Graphics.Vty
import qualified ZTree

type ResourceName = String

data FocusedChild = LeftChild | RightChild deriving (Show)

data State = State
    { ztree              :: ZTree.ZTree Integer
    , focusedChild       :: FocusedChild
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
    where val = ZTree.toString $ ZTree.goRoot $ ztree s

jumokuHandleEvent :: State -> BrickEvent n e -> EventM n (Next State)
jumokuHandleEvent s (VtyEvent (EvKey (KChar c) [])) = case c of
    'h' -> continue s { focusedChild = LeftChild }
    'l' -> continue s { focusedChild = RightChild }
    'k' -> continue s { ztree = ZTree.goUp $ ztree s }
    'j' -> continue s { ztree = moveFn $ ztree s }
        where moveFn = case focusedChild s of
                LeftChild  -> ZTree.goLeft
                RightChild -> ZTree.goRight
    'i' -> continue s { ztree = createFn (ztree s) 7 }
        where createFn = case focusedChild s of
                LeftChild  -> ZTree.createLeft
                RightChild -> ZTree.createRight
    'D' -> continue s { ztree = truncFn $ ztree s }
        where truncFn = case focusedChild s of
                LeftChild  -> ZTree.truncateLeft
                RightChild -> ZTree.truncateRight
    'g' -> continue s { ztree = ZTree.goRoot $ ztree s }
    'q' -> halt s
    _   -> continue s
jumokuHandleEvent s _ = continue s

jumokuInitialState :: IO State
jumokuInitialState = pure State
    { ztree        = ZTree.starterZTree
    , focusedChild = LeftChild
    }

runJumokuApp :: IO ()
runJumokuApp = do
    initalState <- jumokuInitialState
    endState <- defaultMain jumokuApp initalState
    print endState

