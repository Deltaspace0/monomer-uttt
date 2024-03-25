{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Monomer
import Monomer.Checkerboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ AppModel{..} = tree where
    tree = hstack_ [childSpacing_ 16]
        [ zstack
            [ checkerboard_ 3 3 [lightColor gray] checkerWidgets
            , widgetIf (_utttWinner /= PlayerNone) $ getImage _utttWinner
                `styleBasic` [bgColor $ rgba 0 0 0 0.64]
            ] `styleBasic` [sizeReqW $ fixedSize 608]
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ button "Reset game" AppResetGame `nodeEnabled`
                null _amResponseThread
            , if null _amResponseThread
                then button "Play MCTS response" AppRespond
                else button "Abort response" AppAbortResponse
            , label $ "MCTS runs: " <> showt _amMctsRuns
            , hslider_ mctsRuns 100 20000 [dragRate 1]
            , labeledCheckbox_ "Auto reply" autoReply [textRight]
            ]
        ] `styleBasic` [padding 16]
    checkerWidgets = zipWith makeSmallStack [0..] _utttPosition
    makeSmallStack i TTT{..} = zstack
        [ checkerboard_ 3 3 cfg $ zipWith f [0..] _tttPosition
        , widgetIf (_tttWinner /= PlayerNone || i `notElem` _utttLegals) $
            getImage _tttWinner `styleBasic` [bgColor $ rgba 0 0 0 0.64]
        ] where
            f j p = box_ [onBtnPressed $ \_ _ -> AppClick (i, j) True] $
                getImage p
            cfg = if even i
                then [lightColor gray]
                else [lightColor darkBlue, darkColor aqua]
    getImage p = case p of
        PlayerX -> image_ "assets/x.png" [fitEither]
        PlayerO -> image_ "assets/o.png" [fitEither]
        PlayerNone -> filler
    UTTT{..} = _amMainBoard
