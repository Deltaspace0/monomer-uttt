{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.Lens
import Data.Maybe
import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGame
    | AppClick Int Bool
    | AppRespond
    | AppResponseCalculated (Maybe Int)
    | AppSetResponseThread (Maybe ThreadId)
    | AppAbortResponse
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGame -> resetGameHandle model
    AppClick i human -> clickHandle i human model
    AppRespond -> respondHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppSetResponseThread v -> setResponseThreadHandle v model
    AppAbortResponse -> abortResponseHandle model

resetGameHandle :: EventHandle
resetGameHandle model =
    [ Model $ model
        & mainBoard .~ initTTT
        & currentTurn .~ True
    ]

clickHandle :: Int -> Bool -> EventHandle
clickHandle i human model@(AppModel{..}) = response where
    response = if valid
        then
            [ Model $ model
                & mainBoard %~ makeMove p i
                & currentTurn %~ not
            , responseIf (human && _amAutoReply) $ Event AppRespond
            ]
        else []
    valid = and
        [ i `elem` getEmptySquares _amMainBoard
        , not human || null _amResponseThread
        ]
    p = if _amCurrentTurn
        then PlayerX
        else PlayerO

respondHandle :: EventHandle
respondHandle AppModel{..} = [Producer producerHandler] where
    producerHandler raiseEvent = do
        mvar <- newEmptyMVar
        thread <- forkIO $ do
            let p = if _amCurrentTurn
                    then PlayerX
                    else PlayerO
            result <- mctsMove (_amMainBoard, p) _amMctsRuns
            raiseEvent $ AppResponseCalculated result
            putMVar mvar ()
        raiseEvent $ AppSetResponseThread $ Just thread
        takeMVar mvar

responseCalculatedHandle :: Maybe Int -> EventHandle
responseCalculatedHandle v model = response where
    response = (Model $ model & responseThread .~ Nothing):clickEvent
    clickEvent = [Event $ AppClick (fromJust v) False | isJust v]

setResponseThreadHandle :: Maybe ThreadId -> EventHandle
setResponseThreadHandle v model = [Model $ model & responseThread .~ v]

abortResponseHandle :: EventHandle
abortResponseHandle model@(AppModel{..}) =
    [ Model $ model & responseThread .~ Nothing
    , Producer $ const $ maybe (pure ()) killThread _amResponseThread
    ]
