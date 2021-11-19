module GUI where

import Control.Monad (void, replicateM)
import Data.Maybe
import qualified Data.Map as Map
import Text.Printf
--import Safe          (readMay)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import Game

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

    resultApp           <- UI.span
    result              <- UI.span

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}


    button <- UI.button # set UI.text "Start game"


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
                            row [UI.span # set text "Formula approximately evaluates to: ", element resultApp],
                            row [UI.span # set text "Formula evaluates to: ", element result ]
                         ]
                   # set style [
                     ("background","lightskyblue"),
                     ("text-align", "left"),
                     ("color","red"),
                     ("font-size","11px")
                     ]

    let startGameFunction = do

                       domain_size' <- field_domain # get UI.value
                       k' <- field_k # get UI.value
                       m' <- field_m # get UI.value
                       samplesize' <- field_samplesize # get UI.value
                       card1' <- field_card1 # get UI.value
                       card2' <- field_card2 # get UI.value
                       card3' <- field_card3 # get UI.value

                       mode <- dropdown # get UI.value

                       let domain_size = read domain_size'
                           k = read k' :: Int
                           m = read m' :: Int
                           samplesize = read samplesize' :: Int
                           card1 = read card1' :: Int
                           card2 = read card2' :: Int
                           card3 = read card3' :: Int
                           interpScope = [[x] | x <- [0..card1 - 1]] ++ [[x] | x <- [card1 .. (card1 + card2) - 1]]
                           interpRange = [[x] | x <- [0..card1 - 1]] ++ [[x] | x <- [(card1 + card2) .. (card1 + card2 + card3) - 1]]
                           interpretation = Map.insert "S" interpScope $ Map.singleton "R" interpRange :: Interpretation

                       case mode of
                         "\"with repetition\"" -> do
                                      let formula = Quant WR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                          prop = (fromIntegral (card1 + card2)) / (fromIntegral domain_size) :: Double
                                      v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                      element resultApp # set UI.text (show $ 1 - v / (fromIntegral samplesize))
                                      element result # set UI.text (show $ valWR (fromIntegral k) (fromIntegral m) prop)
                         "\"without repetition\"" -> do
                                      let formula = Quant WOR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                          prop = (fromIntegral (card1 + card2)) / (fromIntegral domain_size) :: Double
                                      v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                      element resultApp # set UI.text (show $ 1 - v / (fromIntegral samplesize))
                                      element result # set UI.text (show $ valWOR (fromIntegral domain_size) (fromIntegral k) (fromIntegral m) prop)
                         x -> error x

    on UI.click button $ const $ startGameFunction

{-----------------------------------------------------------------------------
   Dropdown menu
------------------------------------------------------------------------------}
selection :: UI (Element)
selection = do
 let select options = UI.select
       # set style [("display","inline-block"), ("width", "161px")]
       #+ fmap (\x -> UI.option # set UI.text (show x)) options
 mainBox    <- select ["with repetition", "without repetition"]
 return mainBox

{-----------------------------------------------------------------------------
   Functionality
------------------------------------------------------------------------------}
