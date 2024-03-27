{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Model.UTTT
    ( module Model.TTT
    , UTTT(..)
    , initUTTT
    , makeUltimateMove
    ) where

import Control.Lens
import Data.Bits
import Data.Vector (Vector, (!))

import Model.TTT
import Model.MCTS

data UTTT = UTTT
    { _utttPosition :: [TTT]
    , _utttPositionX :: Int
    , _utttPositionO :: Int
    , _utttLegals :: [Int]
    , _utttWinner :: Player
    } deriving (Eq, Show)

instance MCTSGame (UTTT, Player) (Int, Int) where
    getLegalMoves (UTTT{..}, _) = _utttLegals >>= \i ->
        [(i, j) | j <- _tttEmptySquares $ _utttPosition!!i]
    performMove (t, PlayerX) m = (makeUltimateMove PlayerX m t, PlayerO)
    performMove (t, PlayerO) m = (makeUltimateMove PlayerO m t, PlayerX)
    performMove (t, p) m = (makeUltimateMove p m t, p)
    getOutcome (UTTT{..}, p)
        | _utttWinner == PlayerNone = RolloutDraw
        | _utttWinner == p = RolloutWin
        | otherwise = RolloutLoss

initUTTT :: UTTT
initUTTT = UTTT
    { _utttPosition = replicate 9 initTTT
    , _utttPositionX = 0
    , _utttPositionO = 0
    , _utttLegals = [0..8]
    , _utttWinner = PlayerNone
    }

makeUltimateMove :: Player -> (Int, Int) -> UTTT -> UTTT
makeUltimateMove p (i, j) UTTT{..} = result where
    result = UTTT
        { _utttPosition = newPosition
        , _utttPositionX = newPositionX
        , _utttPositionO = newPositionO
        , _utttLegals = if newWinner == PlayerNone
            then newLegals
            else []
        , _utttWinner = if null newLegals && newWinner == PlayerNone
            then winnerByMajority
            else newWinner
        }
    newPosition = _utttPosition & ix i %~ makeMove p j
    newPositionX = if p == PlayerX && _tttWinner (newPosition!!i) == PlayerX
        then _utttPositionX .|. bit i
        else _utttPositionX
    newPositionO = if p == PlayerO && _tttWinner (newPosition!!i) == PlayerO
        then _utttPositionO .|. bit i
        else _utttPositionO
    newLegals = if isFree (newPosition!!j)
        then [j]
        else [x | x <- [0..8], isFree $ newPosition!!x]
    isFree x = not $ null $ _tttEmptySquares x
    newWinner
        | checkerTable!newPositionX = PlayerX
        | checkerTable!newPositionO = PlayerO
        | otherwise = PlayerNone
    winnerByMajority
        | xCount > oCount = PlayerX
        | xCount < oCount = PlayerO
        | otherwise = PlayerNone
    xCount = popCount newPositionX
    oCount = popCount newPositionO
