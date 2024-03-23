{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.TTT
    , AppModel(..)
    , mainBoard
    , currentTurn
    , initModel
    ) where

import Control.Lens

import Model.TTT

data AppModel = AppModel
    { _amMainBoard :: TTT
    , _amCurrentTurn :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amMainBoard = initTTT
    , _amCurrentTurn = True
    }
