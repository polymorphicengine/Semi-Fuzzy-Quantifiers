module GUI where

import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Const)

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Currency Converter"

    scopeP <- UI.input
    rangeP  <- UI.input
    play <- UI.button

    getBody window #+ [
            column [
                grid [[string "A:", element scopeP]
                     ,[string "B:"  , element rangeP ]]
            , element play
            , string "Amounts update while typing."
            ]]
