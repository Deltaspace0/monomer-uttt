{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Monomer
import Monomer.Checkerboard

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ AppModel{..} = tree where
    tree = hstack_ [childSpacing_ 16]
        [ zstack
            [ checkerboard_ 3 3 [lightColor gray] checkerWidgets
            , widgetIf (_tttWinner /= PlayerNone) $ getImage _tttWinner
                `styleBasic` [bgColor $ rgba 0 0 0 0.5]
            ] `styleBasic` [sizeReqW $ fixedSize 608]
        , separatorLine
        , vstack [button "Reset game" AppResetGame]
        ] `styleBasic` [padding 16]
    checkerWidgets = zipWith f [0..] _tttPosition
    f i p = box_ [onBtnPressed $ \_ _ -> AppClick i] $ getImage p
    getImage p = case p of
        PlayerX -> image_ "assets/x.png" [fitEither]
        PlayerO -> image_ "assets/o.png" [fitEither]
        PlayerNone -> filler
    TTT{..} = _amMainBoard
