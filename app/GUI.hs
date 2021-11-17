module GUI where

import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Const)

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Semi-Fuzzy-Quantifiers"

    scopeIn <- UI.input
    rangeIn  <- UI.input
    playButton <- UI.button
                      # set UI.text "Play!"
    display <- UI.pre

    getBody window #+ [
            column [
                grid [[string "Scope:", element scopeIn]
                     ,[string "Range:"  , element rangeIn]]
            , element playButton
            , element display
            ]]

    on UI.click playButton $ \_ -> playButtonFunction scopeIn rangeIn display


playButtonFunction ::  Element -> Element -> Element -> UI ()
playButtonFunction scopeIn rangeIn display = do
                                sc <- scopeIn # get UI.value
                                rn <- rangeIn # get UI.value
                                void $ element display # set UI.text sc
