{-# LANGUAGE RecordWildCards #-}

module Model.AppEvent
    ( ClickMove(..)
    , AppEvent(..)
    , handleEvent
    ) where

import Control.Concurrent
import Control.Lens
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

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
    | AppSetResponseLock (Maybe (MVar ()))
    | AppForceResponse
    | AppSetStatusMessage (Maybe Text)
    | AppSetPreTreeUltimate (Maybe (Tree (UTTT, Player) (Int, Int)))
    | AppSetPreTree (Maybe (Tree (TTT, Player) Int))
    | AppSwitch
    deriving Eq

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGame -> resetGameHandle model
    AppClick m human -> clickHandle m human model
    AppRespond -> respondHandle model
    AppResponseCalculated v -> responseCalculatedHandle v model
    AppSetResponseLock v -> setResponseLockHandle v model
    AppForceResponse -> forceResponseHandle model
    AppSetStatusMessage v -> setStatusMessageHandle v model
    AppSetPreTreeUltimate v -> setPreTreeUltimateHandle v model
    AppSetPreTree v -> setPreTreeHandle v model
    AppSwitch -> switchHandle model

resetGameHandle :: EventHandle
resetGameHandle model =
    [ Model $ model
        & mainBoardUltimate .~ initUTTT
        & mainBoard .~ initTTT
        & currentTurnUltimate .~ True
        & currentTurn .~ True
        & preTreeUltimate .~ Nothing
        & preTree .~ Nothing
    ]

clickHandle :: ClickMove -> Bool -> EventHandle
clickHandle (UltimateMove (i, j)) human model@(AppModel{..}) = response where
    response = if valid
        then
            [ Model $ model
                & mainBoardUltimate %~ makeUltimateMove p (i, j)
                & currentTurnUltimate %~ not
                & preTreeUltimate %~ (>>= getSubTree (i, j))
            , responseIf (human && _amAutoReply) $ Event AppRespond
            , responseIf (not human && _amAutoSwitch) $ Event AppSwitch
            ]
        else []
    valid = and
        [ i `elem` _utttLegals
        , j `elem` _tttEmptySquares (_utttPosition!!i)
        , not human || null _amResponseLock
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
                & preTree %~ (>>= getSubTree i)
            , responseIf (human && _amAutoReply) $ Event AppRespond
            , responseIf (not human && _amAutoSwitch) $ Event AppSwitch
            ]
        else []
    valid = and
        [ i `elem` _tttEmptySquares _amMainBoard
        , not human || null _amResponseLock
        ]
    p = if _amCurrentTurn
        then PlayerX
        else PlayerO

respondHandle :: EventHandle
respondHandle AppModel{..} = [Producer producerHandler] where
    producerHandler raiseEvent = do
        mvar <- newEmptyMVar
        refIterations <- newIORef (0 :: Int)
        let MCTSParameters{..} = _amMctsParams!!_amParamIndex
            initializeUltimateTree = initializeTree
                :: (UTTT, Player) -> Tree (UTTT, Player) (Int, Int)
            initializeSimpleTree = initializeTree
                :: (TTT, Player) -> Tree (TTT, Player) Int
            getUltimateMove = getBestMove
                :: Tree (UTTT, Player) (Int, Int) -> Maybe (Int, Int)
            getSimpleMove = getBestMove
                :: Tree (TTT, Player) Int -> Maybe Int
            positionUltimate = if _amCurrentTurnUltimate
                then (_amMainBoardUltimate, PlayerX)
                else (_amMainBoardUltimate, PlayerO)
            positionSimple = if _amCurrentTurn
                then (_amMainBoard, PlayerX)
                else (_amMainBoard, PlayerO)
            initUltimate = if _mpPreserveTree && (isJust _amPreTreeUltimate)
                then fromJust _amPreTreeUltimate
                else initializeUltimateTree positionUltimate
            initSimple = if _mpPreserveTree && (isJust _amPreTree)
                then fromJust _amPreTree
                else initializeSimpleTree positionSimple
        refUltimate <- newIORef initUltimate
        refSimple <- newIORef initSimple
        let mctsLoop :: (MCTSGame a b) => Int -> IORef (Tree a b) -> IO ()
            mctsLoop 0 _ = putMVar mvar ()
            mctsLoop runs refTree = do
                tree <- readIORef refTree
                newTree <- monteCarloTreeSearch tree _mpMctsTemperature
                writeIORef refTree newTree
                modifyIORef refIterations succ
                mctsLoop (runs-1) refTree
            ultimateLoop = mctsLoop _mpMctsRuns refUltimate
            simpleLoop = mctsLoop _mpMctsRuns refSimple
        thread <- forkIO $ case _amGameMode of
            UTTTMode -> ultimateLoop
            TTTMode -> simpleLoop
        raiseEvent $ AppSetResponseLock $ Just mvar
        takeMVar mvar
        killThread thread
        ultimateTree <- readIORef refUltimate
        simpleTree <- readIORef refSimple
        raiseEvent $ case _amGameMode of
            UTTTMode -> AppSetPreTreeUltimate $ Just ultimateTree
            TTTMode -> AppSetPreTree $ Just simpleTree
        raiseEvent $ AppResponseCalculated $ case _amGameMode of
            UTTTMode -> UltimateMove <$> getUltimateMove ultimateTree
            TTTMode -> SimpleMove <$> getSimpleMove simpleTree
        n <- readIORef refIterations
        let iterationMessage = "Completed " <> (showt n) <> " iterations"
        raiseEvent $ AppSetStatusMessage $ Just iterationMessage

responseCalculatedHandle :: Maybe ClickMove -> EventHandle
responseCalculatedHandle v model = response where
    response = (Model $ model & responseLock .~ Nothing):clickEvent
    clickEvent = [Event $ AppClick (fromJust v) False | isJust v]

setResponseLockHandle :: Maybe (MVar ()) -> EventHandle
setResponseLockHandle v model = [Model $ model & responseLock .~ v]

forceResponseHandle :: EventHandle
forceResponseHandle model@(AppModel{..}) =
    [ Model $ model & responseLock .~ Nothing
    , Producer $ const $ maybe (pure ()) (flip putMVar ()) _amResponseLock
    ]

setStatusMessageHandle :: Maybe Text -> EventHandle
setStatusMessageHandle v model = [Model $ model & statusMessage .~ v]

setPreTreeUltimateHandle
    :: Maybe (Tree (UTTT, Player) (Int, Int))
    -> EventHandle
setPreTreeUltimateHandle v model = [Model $ model & preTreeUltimate .~ v]

setPreTreeHandle :: Maybe (Tree (TTT, Player) Int) -> EventHandle
setPreTreeHandle v model = [Model $ model & preTree .~ v]

switchHandle :: EventHandle
switchHandle model = [Model $ model & paramIndex %~ (1-)]
