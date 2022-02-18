module Render (runJumokuApp) where

import Brick
import Graphics.Vty
import qualified BTree

type ResourceName = String

data State = State
    { tree              :: BTree.BTree Integer
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
jumokuDraw s = [ vBox [ str "test" ] ]

jumokuHandleEvent :: State -> BrickEvent n e -> EventM n (Next State)
jumokuHandleEvent s (VtyEvent (EvKey (KChar c) [])) = case c of
    'q' -> halt s
jumokuHandleEvent s _ = continue s

jumokuInitialState :: IO State
jumokuInitialState = pure State
    { tree = BTree.Empty
    }

runJumokuApp :: IO ()
runJumokuApp = do
    initalState <- jumokuInitialState
    endState <- defaultMain jumokuApp initalState
    print endState

