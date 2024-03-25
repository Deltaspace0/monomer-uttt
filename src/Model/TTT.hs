{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Model.TTT
    ( Player(..)
    , TTT(..)
    , initTTT
    , getEmptySquares
    , makeMove
    , checkWinner
    ) where

import Control.Lens

import Model.MCTS

data Player
    = PlayerX
    | PlayerO
    | PlayerNone
    deriving (Eq, Show)

data TTT = TTT
    { _tttPosition :: [Player]
    , _tttWinner :: Player
    } deriving (Eq, Show)

instance MCTSGame (TTT, Player) Int where
    getLegalMoves (t, _) = getEmptySquares t
    performMove (t, PlayerX) i = (makeMove PlayerX i t, PlayerO)
    performMove (t, PlayerO) i = (makeMove PlayerO i t, PlayerX)
    performMove (t, p) i = (makeMove p i t, p)
    getOutcome (TTT{..}, p)
        | _tttWinner == PlayerNone = RolloutDraw
        | _tttWinner == p = RolloutWin
        | otherwise = RolloutLoss

initTTT :: TTT
initTTT = TTT
    { _tttPosition = replicate 9 PlayerNone
    , _tttWinner = PlayerNone
    }

getEmptySquares :: TTT -> [Int]
getEmptySquares TTT{..} = if _tttWinner == PlayerNone
    then [i | i <- [0..8], _tttPosition!!i == PlayerNone]
    else []

makeMove :: Player -> Int -> TTT -> TTT
makeMove p i TTT{..} = result where
    result = TTT
        { _tttPosition = newPosition
        , _tttWinner = checkWinner newPosition
        }
    newPosition = _tttPosition & ix i .~ p

checkWinner :: [Player] -> Player
checkWinner position = result where
    result = f
        [ [0, 1, 2]
        , [3, 4, 5]
        , [6, 7, 8]
        , [0, 3, 6]
        , [1, 4, 7]
        , [2, 5, 8]
        , [0, 4, 8]
        , [2, 4, 6]
        ]
    f [] = PlayerNone
    f (x:xs) = if checkLine x == PlayerNone
        then f xs
        else checkLine x
    checkLine line = case (position!!) <$> line of
        [PlayerX, PlayerX, PlayerX] -> PlayerX
        [PlayerO, PlayerO, PlayerO] -> PlayerO
        _ -> PlayerNone
