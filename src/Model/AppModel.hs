module Model.AppModel
    ( AppModel(..)
    , initModel
    ) where

data AppModel = AppModel () deriving (Eq, Show)

initModel :: AppModel
initModel = AppModel ()
