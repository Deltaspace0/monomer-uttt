module Main (main) where

import Monomer
import qualified Monomer.Lens as L

import Model
import UI

main :: IO ()
main = do
    let config =
            [ appWindowState $ MainWindowNormal (900, 640)
            , appWindowResizable False
            , appWindowTitle "Monomer UTTT"
            , appTheme $ setThemeValue L.scrollWheelRate 32 $ darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
    startApp initModel handleEvent buildUI config
