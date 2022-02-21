module Cli where

import System.Environment

cli :: IO ()
cli = do
    args <- getArgs
    mapM_ putStrLn args
