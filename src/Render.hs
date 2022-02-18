module Render (runJumokuApp) where

import Brick
import BTree

type ResourceName = String

data State = State
    {

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
jumokuHandleEvent s e =
    case e of
        _ -> continue s

jumokuInitialState :: IO State
jumokuInitialState = pure State { }

runJumokuApp :: IO ()
runJumokuApp = do
    initalState <- jumokuInitialState
    endState <- defaultMain jumokuApp initalState
    print endState

