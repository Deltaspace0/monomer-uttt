{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGame
    | AppClick Int
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGame -> resetGameHandle model
    AppClick i -> clickHandle i model

resetGameHandle :: EventHandle
resetGameHandle model =
    [ Model $ model
        & mainBoard .~ initTTT
        & currentTurn .~ True
    ]

clickHandle :: Int -> EventHandle
clickHandle i model@(AppModel{..}) = response where
    response = if i `elem` getEmptySquares _amMainBoard
        then
            [ Model $ model
                & mainBoard %~ makeMove p i
                & currentTurn %~ not
            ]
        else []
    p = if _amCurrentTurn
        then PlayerX
        else PlayerO
