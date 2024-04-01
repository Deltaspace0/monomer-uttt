{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.MCTS
    , module Model.UTTT
    , MCTSParameters(..)
    , GameMode(..)
    , AppModel(..)
    , mctsRuns
    , mctsTemperature
    , preserveTree
    , mainBoardUltimate
    , mainBoard
    , currentTurnUltimate
    , currentTurn
    , responseLock
    , autoReply
    , gameMode
    , statusMessage
    , preTreeUltimate
    , preTree
    , mctsParams
    , paramIndex
    , autoSwitch
    , initModel
    ) where

import Control.Concurrent
import Control.Lens
import Data.Text (Text)

import Model.MCTS
import Model.UTTT

data MCTSParameters = MCTSParameters
    { _mpMctsRuns :: Int
    , _mpMctsTemperature :: Double
    , _mpPreserveTree :: Bool
    } deriving Eq

data GameMode
    = UTTTMode
    | TTTMode
    deriving (Eq, Show)

data AppModel = AppModel
    { _amMainBoardUltimate :: UTTT
    , _amMainBoard :: TTT
    , _amCurrentTurnUltimate :: Bool
    , _amCurrentTurn :: Bool
    , _amResponseLock :: Maybe (MVar ())
    , _amAutoReply :: Bool
    , _amGameMode :: GameMode
    , _amStatusMessage :: Maybe Text
    , _amPreTreeUltimate :: Maybe (Tree (UTTT, Player) (Int, Int))
    , _amPreTree :: Maybe (Tree (TTT, Player) Int)
    , _amMctsParams :: [MCTSParameters]
    , _amParamIndex :: Int
    , _amAutoSwitch :: Bool
    } deriving Eq

makeLensesWith abbreviatedFields 'MCTSParameters
makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amMainBoardUltimate = initUTTT
    , _amMainBoard = initTTT
    , _amCurrentTurnUltimate = True
    , _amCurrentTurn = True
    , _amResponseLock = Nothing
    , _amAutoReply = True
    , _amGameMode = UTTTMode
    , _amStatusMessage = Nothing
    , _amPreTreeUltimate = Nothing
    , _amPreTree = Nothing
    , _amMctsParams = [initParam, initParam]
    , _amParamIndex = 0
    , _amAutoSwitch = True
    }

initParam :: MCTSParameters
initParam = MCTSParameters
    { _mpMctsRuns = 5000
    , _mpMctsTemperature = 0.25
    , _mpPreserveTree = True
    }
