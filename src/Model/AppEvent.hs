{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( ClickMove(..)
    , AppEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.Lens
import Data.Maybe
import Monomer

import Model.AppModel

data ClickMove
    = UltimateMove (Int, Int)
    | SimpleMove Int
    deriving (Eq, Show)

data AppEvent
    = AppInit
    | AppResetGame
    | AppClick ClickMove Bool
    | AppRespond
    | AppResponseCalculated (Maybe ClickMove)
    | AppSetResponseThread (Maybe ThreadId)
    | AppAbortResponse
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGame -> resetGameHandle model
    AppClick m human -> clickHandle m human model
    AppRespond -> respondHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppSetResponseThread v -> setResponseThreadHandle v model
    AppAbortResponse -> abortResponseHandle model

resetGameHandle :: EventHandle
resetGameHandle model =
    [ Model $ model
        & mainBoardUltimate .~ initUTTT
        & mainBoard .~ initTTT
        & currentTurnUltimate .~ True
        & currentTurn .~ True
    ]

clickHandle :: ClickMove -> Bool -> EventHandle
clickHandle (UltimateMove (i, j)) human model@(AppModel{..}) = response where
    response = if valid
        then
            [ Model $ model
                & mainBoardUltimate %~ makeUltimateMove p (i, j)
                & currentTurnUltimate %~ not
            , responseIf (human && _amAutoReply) $ Event AppRespond
            ]
        else []
    valid = and
        [ i `elem` _utttLegals
        , j `elem` getEmptySquares (_utttPosition!!i)
        , not human || null _amResponseThread
        ]
    p = if _amCurrentTurnUltimate
        then PlayerX
        else PlayerO
    UTTT{..} = _amMainBoardUltimate
clickHandle (SimpleMove i) human model@(AppModel{..}) = response where
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
        let ultimateSequence = do
                let p = if _amCurrentTurnUltimate
                        then PlayerX
                        else PlayerO
                result <- mctsMove (_amMainBoardUltimate, p) _amMctsRuns
                raiseEvent $ AppResponseCalculated $ UltimateMove <$> result
                putMVar mvar ()
            simpleSequence = do
                let p = if _amCurrentTurn
                        then PlayerX
                        else PlayerO
                result <- mctsMove (_amMainBoard, p) _amMctsRuns
                raiseEvent $ AppResponseCalculated $ SimpleMove <$> result
                putMVar mvar ()
        thread <- forkIO $ case _amGameMode of
            UTTTMode -> ultimateSequence
            TTTMode -> simpleSequence
        raiseEvent $ AppSetResponseThread $ Just thread
        takeMVar mvar

responseCalculatedHandle :: Maybe ClickMove -> EventHandle
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
