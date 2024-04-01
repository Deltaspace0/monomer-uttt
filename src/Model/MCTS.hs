{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MCTS
    ( RolloutOutcome(..)
    , MCTSGame
    , Tree(..)
    , getLegalMoves
    , performMove
    , getOutcome
    , mctsMove
    , getBestMove
    , monteCarloTreeSearch
    , initializeTree
    , getSubTree
    ) where

import Control.Lens
import Data.Maybe
import System.Random

data RolloutOutcome
    = RolloutWin
    | RolloutLoss
    | RolloutDraw
    deriving (Eq, Show)

class MCTSGame a b | a -> b where
    getLegalMoves :: a -> [b]
    performMove :: a -> b -> a
    getOutcome :: a -> RolloutOutcome

data Tree a b = Tree
    { _tRootPosition :: a
    , _tStatWins :: Double
    , _tStatSims :: Int
    , _tChildNodes :: [(Tree a b, b)]
    }

instance Eq (Tree a b) where
    _ == _ = False

mctsMove :: (MCTSGame a b) => a -> Double -> Int -> IO (Maybe b)
mctsMove position temp runs = do
    finalTree <- mctsRepeat temp runs $ initializeTree position
    return $ getBestMove finalTree

getBestMove :: (MCTSGame a b) => Tree a b -> Maybe b
getBestMove tree = snd <$> getBestNode (_tChildNodes tree)

getBestNode :: (MCTSGame a b) => [(Tree a b, b)] -> Maybe (Tree a b, b)
getBestNode [] = Nothing
getBestNode (x:xs) = result where
    result = Just $ if null other || sims > otherSims
        then x
        else fromJust other
    sims = _tStatSims $ fst x
    otherSims = _tStatSims $ fst $ fromJust other
    other = getBestNode xs

mctsRepeat :: (MCTSGame a b) => Double -> Int -> Tree a b -> IO (Tree a b)
mctsRepeat temp n tree = if n <= 0
    then pure tree
    else monteCarloTreeSearch tree temp >>= mctsRepeat temp (n-1)

monteCarloTreeSearch :: (MCTSGame a b) => Tree a b -> Double -> IO (Tree a b)
monteCarloTreeSearch tree@(Tree root wins sims nodes) temp = result where
    result = if sims == 0 || null nodes
        then doRollout root <&> \x -> tree
            { _tStatWins = wins + x
            , _tStatSims = sims + 1
            }
        else monteCarloTreeSearch subTree temp <&> \x -> tree
            { _tStatWins = 1 + wins + subWins - _tStatWins x
            , _tStatSims = sims + 1
            , _tChildNodes = nodes & ix i . _1 .~ x
            }
    (subTree@(Tree _ subWins _ _), i, _) = selectChild logSims pairs
    logSims = temp*(log $ fromIntegral sims)
    pairs = zip (fst <$> nodes) [0..]

selectChild :: Double -> [(Tree a b, Int)] -> (Tree a b, Int, Double)
selectChild _ [] = error "No child nodes to select"
selectChild logSims nodes@((tree@(Tree _ w s _), move):xs)
    | length nodes == 1 || s == 0 = child
    | otherEval == -1 || eval < otherEval = otherChild
    | otherwise = child
    where
        child = (tree, move, eval)
        eval = if s == 0
            then -1
            else w/s' + sqrt (logSims/s')
        s' = fromIntegral s
        otherChild@(_, _, otherEval) = selectChild logSims xs

doRollout :: (MCTSGame a b) => a -> IO Double
doRollout position
    | null legal && outcome == RolloutWin = pure 0
    | null legal && outcome == RolloutLoss = pure 1
    | null legal = pure 0.5
    | otherwise = (1-) <$> rollout
    where
        legal = getLegalMoves position
        outcome = getOutcome position
        rollout = i >>= doRollout . performMove position . (legal!!)
        i = randomRIO (0, length legal-1)

initializeTree :: (MCTSGame a b) => a -> Tree a b
initializeTree position = Tree
    { _tRootPosition = position
    , _tStatWins = 0
    , _tStatSims = 0
    , _tChildNodes = (\x -> (subTree x, x)) <$> getLegalMoves position
    } where
        subTree = initializeTree . performMove position

getSubTree :: (Eq b) => b -> Tree a b -> Maybe (Tree a b)
getSubTree move Tree{..} = findNode _tChildNodes where
    findNode [] = Nothing
    findNode ((subTree, x):xs) = if x == move
        then Just subTree
        else findNode xs
