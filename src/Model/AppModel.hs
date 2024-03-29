{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.MCTS
    , module Model.UTTT
    , GameMode(..)
    , AppModel(..)
    , mainBoardUltimate
    , mainBoard
    , currentTurnUltimate
    , currentTurn
    , mctsRuns
    , responseLock
    , autoReply
    , gameMode
    , statusMessage
    , preserveTree
    , preTreeUltimate
    , preTree
    , initModel
    ) where

import Control.Concurrent
import Control.Lens
import Data.Text (Text)

import Model.MCTS
import Model.UTTT

data GameMode
    = UTTTMode
    | TTTMode
    deriving (Eq, Show)

data AppModel = AppModel
    { _amMainBoardUltimate :: UTTT
    , _amMainBoard :: TTT
    , _amCurrentTurnUltimate :: Bool
    , _amCurrentTurn :: Bool
    , _amMctsRuns :: Int
    , _amResponseLock :: Maybe (MVar ())
    , _amAutoReply :: Bool
    , _amGameMode :: GameMode
    , _amStatusMessage :: Maybe Text
    , _amPreserveTree :: Bool
    , _amPreTreeUltimate :: Maybe (Tree (UTTT, Player) (Int, Int))
    , _amPreTree :: Maybe (Tree (TTT, Player) Int)
    } deriving Eq

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amMainBoardUltimate = initUTTT
    , _amMainBoard = initTTT
    , _amCurrentTurnUltimate = True
    , _amCurrentTurn = True
    , _amMctsRuns = 5000
    , _amResponseLock = Nothing
    , _amAutoReply = True
    , _amGameMode = UTTTMode
    , _amStatusMessage = Nothing
    , _amPreserveTree = True
    , _amPreTreeUltimate = Nothing
    , _amPreTree = Nothing
    }
