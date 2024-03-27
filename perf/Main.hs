module Main (main) where

import Model.MCTS
import Model.UTTT

main :: IO ()
main = do
    let game = initUTTT
    result <- mctsMove (game, PlayerX) 10000
    print result
