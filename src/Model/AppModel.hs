{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.MCTS
    , module Model.UTTT
    , AppModel(..)
    , mainBoard
    , currentTurn
    , mctsRuns
    , responseThread
    , autoReply
    , initModel
    ) where

import Control.Concurrent
import Control.Lens

import Model.MCTS
import Model.UTTT

data AppModel = AppModel
    { _amMainBoard :: UTTT
    , _amCurrentTurn :: Bool
    , _amMctsRuns :: Int
    , _amResponseThread :: Maybe ThreadId
    , _amAutoReply :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amMainBoard = initUTTT
    , _amCurrentTurn = True
    , _amMctsRuns = 2000
    , _amResponseThread = Nothing
    , _amAutoReply = True
    }
