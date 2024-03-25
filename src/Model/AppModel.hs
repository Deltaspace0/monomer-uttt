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
    , responseThread
    , autoReply
    , gameMode
    , initModel
    ) where

import Control.Concurrent
import Control.Lens

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
    , _amResponseThread :: Maybe ThreadId
    , _amAutoReply :: Bool
    , _amGameMode :: GameMode
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amMainBoardUltimate = initUTTT
    , _amMainBoard = initTTT
    , _amCurrentTurnUltimate = True
    , _amCurrentTurn = True
    , _amMctsRuns = 2000
    , _amResponseThread = Nothing
    , _amAutoReply = True
    , _amGameMode = UTTTMode
    }
