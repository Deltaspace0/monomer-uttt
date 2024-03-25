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

import Model.TTT
import Model.MCTS

data UTTT = UTTT
    { _utttPosition :: [TTT]
    , _utttLegals :: [Int]
    , _utttWinner :: Player
    } deriving (Eq, Show)

instance MCTSGame (UTTT, Player) (Int, Int) where
    getLegalMoves (UTTT{..}, _) = _utttLegals >>= \i ->
        [(i, j) | j <- getEmptySquares $ _utttPosition!!i]
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
    , _utttLegals = [0..8]
    , _utttWinner = PlayerNone
    }

makeUltimateMove :: Player -> (Int, Int) -> UTTT -> UTTT
makeUltimateMove p (i, j) UTTT{..} = result where
    result = UTTT
        { _utttPosition = newPosition
        , _utttLegals = if newWinner == PlayerNone
            then newLegals
            else []
        , _utttWinner = if null newLegals && newWinner == PlayerNone
            then winnerByMajority
            else newWinner
        }
    newPosition = _utttPosition & ix i %~ makeMove p j
    newLegals = if isFree (newPosition!!j)
        then [j]
        else [x | x <- [0..8], isFree $ newPosition!!x]
    isFree x = not $ null $ getEmptySquares x
    newWinner = checkWinner $ _tttWinner <$> newPosition
    winnerByMajority
        | xCount > oCount = PlayerX
        | xCount < oCount = PlayerO
        | otherwise = PlayerNone
    xCount = length $ filter ((==PlayerX) . _tttWinner) newPosition
    oCount = length $ filter ((==PlayerO) . _tttWinner) newPosition
