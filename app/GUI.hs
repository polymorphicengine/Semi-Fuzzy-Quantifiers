module GUI where

import Control.Monad (void)
import Data.Maybe
import Text.Printf
--import Safe          (readMay)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- main :: IO ()
-- main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    dropdown <- selection

{-----------------------------------------------------------------------------
    Display fields
------------------------------------------------------------------------------}

    result           <- UI.span

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

    button <- UI.button # set UI.text "Start game"
    on UI.click button $ const $ do
        element button # set UI.text "Wowzie! What a game."
        element result # set UI.text "WHEW, so ist es also um die Wahrheit der Formel bestellt!"

{-----------------------------------------------------------------------------
    Input fields
------------------------------------------------------------------------------}

    field_domain     <- UI.input
    field_k          <- UI.input
    field_m          <- UI.input
    field_samplesize <- UI.input

    field_card1      <- UI.input
    field_card2      <- UI.input
    field_card3      <- UI.input

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}

    getBody window #+ [
                       row [
                            column [
                                   grid [
                                           [element dropdown],
                                           [string "Value k : ", element field_k],
                                           [string "Value m : ", element field_m]
                                        ], UI.hr
                                   ],
                            column [
                                   grid [
                                           [string "Cardinality of Domain : ", element field_domain],
                                           [string "Cardinality of Intersection(Scope,Range) : ", element field_card1],
                                           [string "Cardinality of Scope without Range : ", element field_card2],
                                           [string "Cardinality of Range without Scope : ", element field_card3]
                                        ], UI.hr
                                   ]
                            ],
                            row [element button, element field_samplesize],
                            row [UI.span # set text "Formula evluates to: ", element result]
                         ]
                   # set style [
                     ("background","lightskyblue"),
                     ("text-align", "left"),
                     ("color","red"),
                     ("font-size","17px")
                     ]
                     -- element UI.span # set UI.text "Formula is: Muss ma' sich amal anschaun."

{-----------------------------------------------------------------------------
   Dropdown menu
------------------------------------------------------------------------------}
selection :: UI (Element)
selection = do
 let select options = UI.select
       # set style [("display","inline-block"), ("width", "161px")]
       #+ fmap (\x -> UI.option # set UI.text (show x)) options
     selectionList :: [UI Element]
     selectionList = replicate 1 $ select ["with repetition", "without repetition"]
 selection' <- (sequence selectionList :: UI [Element])
 mainBox    <- UI.mkElement "selectDiv"
   # set style [("display","inline-block"), ("background-color", "transparent"), ("width", "150px"), ("padding", "1px")] --
   #+ (fmap pure selection')
 return mainBox

{-----------------------------------------------------------------------------
   Functionality
------------------------------------------------------------------------------}
