module Main where

import qualified BTree
import qualified Render
import qualified Cli

main :: IO ()
main = Render.runJumokuApp
-- main = Cli.cli
