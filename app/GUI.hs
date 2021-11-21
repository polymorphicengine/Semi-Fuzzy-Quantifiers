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

    mathjaxScript1 <- (UI.mkElement "script")
                # set UI.src "https://polyfill.io/v3/polyfill.min.js?features=es6"

    mathjaxScript2 <- (UI.mkElement "script")
                # set UI.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

    dropdown <- selection

    wr_check <- UI.input # set UI.type_ "checkbox"

    wor_check <- UI.input # set UI.type_ "checkbox"

    wr_input <- UI.label #+ [element wr_check,
                              UI.span # set text "\\( \\Pi^k_m \\)"
                             ]

    wor_input <- UI.label #+ [element wor_check,
                             UI.span # set text "\\( \\Pi^{j,k} \\)"
                            ]

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
                            # set style [("width","20%")]
    field_k          <- UI.input
                            # set style [("width","20%")]
    field_m          <- UI.input
                            # set style [("width","20%")]
    field_samplesize <- UI.input
                            # set style [("width","50%")]

    field_card1      <- UI.input
                           # set style [("width","20%")]
    field_card2      <- UI.input
                           # set style [("width","20%")]
    field_card3      <- UI.input
                           # set style [("width","20%")]

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}

    getBody window #+ [element mathjaxScript1,
                       element mathjaxScript2,
                       UI.p # set text "\\[\\Pi^k_m x . S(x)\\]",
                       row [
                            column [
                                   grid [
                                           [],
                                           [element wr_input, element wor_input],
                                           [string "\\(k:\\)", element field_k],
                                           [string "\\(m:\\)", element field_m]
                                        ],
                                   UI.hr # set style [("width","75%")]
                                   ]
                                   ,
                            column [
                                   grid [
                                           [string "\\( |D| :\\)", element field_domain],
                                           [string "\\( | S \\cap R | :\\)", element field_card1],
                                           [string "\\( | S \\setminus R | :\\)", element field_card2],
                                           [string "\\( | R \\setminus S | :\\)", element field_card3]
                                        ],
                                   UI.hr
                                   ]
                            ],
                        row [element button, element field_samplesize],
                        row [UI.span # set text "Formula approximately evaluates to: ", element resultApp],
                        row [UI.span # set text "Formula evaluates to: ", element result ]
                       ]
                   # set style [
                     ("background","lightskyblue"),
                     ("text-align", "left"),
                     ("color","black"),
                     ("font-size","20px"),
                     ("width","100%")
                     ]

    let startGameFunction = do

                       domain_size' <- field_domain # get UI.value
                       k' <- field_k # get UI.value
                       m' <- field_m # get UI.value
                       samplesize' <- field_samplesize # get UI.value
                       card1' <- field_card1 # get UI.value
                       card2' <- field_card2 # get UI.value
                       card3' <- field_card3 # get UI.value

                       mode <- wr_check # get UI.checked

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
                         True -> do
                                      let formula = Quant WR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                          prop = (fromIntegral (card1 + card2)) / (fromIntegral domain_size) :: Double
                                      v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                      element resultApp # set UI.text (show $ 1 - v / (fromIntegral samplesize))
                                      element result # set UI.text (show $ valWR (fromIntegral k) (fromIntegral m) prop)
                         False -> do
                                      let formula = Quant WOR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                          prop = (fromIntegral (card1 + card2)) / (fromIntegral domain_size) :: Double
                                      v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                      element resultApp # set UI.text (show $ 1 - v / (fromIntegral samplesize))
                                      element result # set UI.text (show $ valWOR (fromIntegral domain_size) (fromIntegral k) (fromIntegral m) prop)

    on UI.click button $ const $ startGameFunction

    on UI.checkedChange wr_check $ const $ do
                                element wor_check # set UI.checked False

    on UI.checkedChange wor_check $ const $ do
                                element wr_check # set UI.checked False

{-----------------------------------------------------------------------------
   Dropdown menu
------------------------------------------------------------------------------}
selection :: UI (Element)
selection =  UI.select
       # set style [("display","inline-block"), ("width", "200px"), ("height", "200px")]
       #+ [UI.option # set value "WR"
                     #+ [UI.div # set text "\\[\\Pi^k_m x . S(x)\\]"
                              # set style [("width", "200px"), ("height", "200px")]
                        ]
                     # set style [("width", "200px"), ("height", "200px")]
       ]


{-----------------------------------------------------------------------------
   Functionality
------------------------------------------------------------------------------}
