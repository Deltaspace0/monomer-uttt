{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Data.Bits
import Data.Maybe
import Monomer
import Monomer.Checkerboard

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ AppModel{..} = tree where
    tree = hstack_ [childSpacing_ 16]
        [ case _amGameMode of
            UTTTMode -> ultimateMode
            TTTMode -> simpleMode
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ button "Reset game" AppResetGame `nodeEnabled` notResponding
            , if notResponding
                then button "Play MCTS response" AppRespond
                else button "Force response" AppForceResponse
            , widgetIf (isJust _amStatusMessage) $
                label $ fromMaybe "" _amStatusMessage
            , separatorLine
            , hstack_ [childSpacing_ 16]
                [ label "MCTS runs:"
                , numericField mctsRuns
                ]
            , hstack_ [childSpacing_ 16]
                [ label "Temperature:"
                , numericField mctsTemperature
                ]
            , labeledCheckbox_ "Preserve tree" preserveTree [textRight]
            , labeledCheckbox_ "Auto reply" autoReply [textRight]
            , separatorLine
            , label "Game mode:"
            , vstack_ [childSpacing_ 16]
                [ optionButton "UTTT" UTTTMode gameMode
                , optionButton "Tic-tac-toe" TTTMode gameMode
                ] `nodeEnabled` notResponding
            ]
        ] `styleBasic` [padding 16]
    notResponding = null _amResponseLock
    ultimateMode = zstack
        [ checkerboard_ 3 3 [lightColor gray] checkerWidgets
        , widgetIf (_utttWinner /= PlayerNone) $ getImage _utttWinner
            `styleBasic` [bgColor $ rgba 0 0 0 0.64]
        ] `styleBasic` [sizeReqW $ fixedSize 608]
    simpleMode = zstack
        [ checkerboard_ 3 3 [lightColor gray] checkerWidgets'
        , widgetIf (simpleWinner /= PlayerNone) $
            getImage simpleWinner `styleBasic` [bgColor $ rgba 0 0 0 0.64]
        ] `styleBasic` [sizeReqW $ fixedSize 608]
    simpleWinner = _tttWinner _amMainBoard
    checkerWidgets = zipWith makeSmallStack [0..] _utttPosition
    makeSmallStack i t@(TTT{..}) = zstack
        [ checkerboard_ 3 3 cfg $ zipWith f [0..] $ restorePosition t
        , widgetIf (_tttWinner /= PlayerNone || i `notElem` _utttLegals) $
            getImage _tttWinner `styleBasic` [bgColor $ rgba 0 0 0 0.64]
        ] where
            f j p = box_ [onBtnPressed $ \_ _ ->
                AppClick (UltimateMove (i, j)) True] $ getImage p
            cfg = if even i
                then [lightColor gray]
                else [lightColor darkBlue, darkColor aqua]
    checkerWidgets' = zipWith f' [0..] $ restorePosition _amMainBoard
    restorePosition TTT{..} = setPlayer <$> [0..8] where
        setPlayer i
            | _tttPositionX `testBit` i = PlayerX
            | _tttPositionO `testBit` i = PlayerO
            | otherwise = PlayerNone
    f' i p = box_ [onBtnPressed $ \_ _ ->
        AppClick (SimpleMove i) True] $ getImage p
    getImage p = case p of
        PlayerX -> image_ "assets/x.png" [fitEither]
        PlayerO -> image_ "assets/o.png" [fitEither]
        PlayerNone -> filler
    UTTT{..} = _amMainBoardUltimate
