{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Model.TTT
    ( Player(..)
    , TTT(..)
    , initTTT
    , makeMove
    , checkerTable
    ) where

import Data.Bits
import Data.Vector (Vector, (!), fromList)

import Model.MCTS

data Player
    = PlayerX
    | PlayerO
    | PlayerNone
    deriving (Eq, Show)

data TTT = TTT
    { _tttPositionX :: Int
    , _tttPositionO :: Int
    , _tttEmptySquares :: [Int]
    , _tttWinner :: Player
    } deriving (Eq, Show)

instance MCTSGame (TTT, Player) Int where
    getLegalMoves (TTT{..}, _) = _tttEmptySquares
    performMove (t, PlayerX) i = (makeMove PlayerX i t, PlayerO)
    performMove (t, PlayerO) i = (makeMove PlayerO i t, PlayerX)
    performMove (t, p) i = (makeMove p i t, p)
    getOutcome (TTT{..}, p)
        | _tttWinner == PlayerNone = RolloutDraw
        | _tttWinner == p = RolloutWin
        | otherwise = RolloutLoss

initTTT :: TTT
initTTT = TTT
    { _tttPositionX = 0
    , _tttPositionO = 0
    , _tttEmptySquares = [0..8]
    , _tttWinner = PlayerNone
    }

makeMove :: Player -> Int -> TTT -> TTT
makeMove p i TTT{..} = result where
    result = TTT
        { _tttPositionX = newPositionX
        , _tttPositionO = newPositionO
        , _tttEmptySquares = if newWinner == PlayerNone
            then filter (/=i) _tttEmptySquares
            else []
        , _tttWinner = newWinner
        }
    newPositionX = if p == PlayerX
        then _tttPositionX .|. bit i
        else _tttPositionX
    newPositionO = if p == PlayerO
        then _tttPositionO .|. bit i
        else _tttPositionO
    newWinner
        | checkerTable!newPositionX = PlayerX
        | checkerTable!newPositionO = PlayerO
        | otherwise = PlayerNone

checkerTable :: Vector Bool
checkerTable = (\x -> any (testMask x) targets) <$> fromList [0..511] where
    testMask x i = x .&. i == i
    targets = [7, 56, 73, 84, 146, 273, 292, 448] :: [Int]
