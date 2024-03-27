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
    , _tStatSimulations :: Int
    , _tChildNodes :: [(Tree a b, b)]
    }

mctsMove :: (MCTSGame a b) => a -> Int -> IO (Maybe b)
mctsMove position runs = do
    finalTree <- mctsRepeat runs $ initializeTree position
    return $ snd <$> getBestNode (_tChildNodes finalTree)

getBestNode :: (MCTSGame a b) => [(Tree a b, b)] -> Maybe (Tree a b, b)
getBestNode [] = Nothing
getBestNode (x:xs) = result where
    result = Just $ if null other || simulations > otherSimulations
        then x
        else fromJust other
    simulations = _tStatSimulations $ fst x
    otherSimulations = _tStatSimulations $ fst $ fromJust other
    other = getBestNode xs

mctsRepeat :: (MCTSGame a b) => Int -> Tree a b -> IO (Tree a b)
mctsRepeat n tree = if n <= 0
    then pure tree
    else monteCarloTreeSearch tree >>= mctsRepeat (n-1)

monteCarloTreeSearch :: (MCTSGame a b) => Tree a b -> IO (Tree a b)
monteCarloTreeSearch tree@(Tree root wins simulations nodes) = result where
    result = if simulations == 0 || null nodes
        then doRollout root <&> \x -> tree
            { _tStatWins = wins + x
            , _tStatSimulations = simulations + 1
            }
        else monteCarloTreeSearch subTree <&> \x -> tree
            { _tStatWins = 1 + wins + subWins - _tStatWins x
            , _tStatSimulations = simulations + 1
            , _tChildNodes = nodes & ix i . _1 .~ x
            }
    (subTree@(Tree _ subWins _ _), i) = selectChild tree

selectChild :: Tree a b -> (Tree a b, Int)
selectChild Tree{..} = f $ zip subTrees [0..] where
    f [] = error "No child nodes to select"
    f elems@(x@(Tree _ w s _, _):xs)
        | length elems == 1 || s == 0 = x
        | eval w s > eval w' s' = x
        | otherwise = x'
        where
            x'@(Tree _ w' s' _, _) = f xs
    eval w s = let s' = fromIntegral s in w/s' + sqrt (n/s')
    n = 2 * (log $ fromIntegral _tStatSimulations)
    subTrees = fst <$> _tChildNodes

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
    , _tStatSimulations = 0
    , _tChildNodes = (\x -> (subTree x, x)) <$> getLegalMoves position
    } where
        subTree = initializeTree . performMove position
