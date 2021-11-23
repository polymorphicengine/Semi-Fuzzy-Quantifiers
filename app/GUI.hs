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

    button <- UI.button # set UI.text " Start game! "
                        # set style [("background-image","linear-gradient(to bottom right, red,yellow)")]

    on UI.hover button $ \_ -> do
        element button # set text "Yes, push me!"

    on UI.leave button $ \_ -> do
        element button # set text " Start game! "

    on UI.click button $ \_ -> do
        element button # set text " Thonk you! "

{-----------------------------------------------------------------------------
    Input fields
------------------------------------------------------------------------------}

    field_domain     <- UI.input
                            # set style [("width","20%"),("text-align","center")]
    field_k          <- UI.input
                            # set style [("width","20%"),("text-align","center")]

    field_m          <- UI.input
                            # set style [("width","20%"), ("text-align", "center")]
    field_samplesize <- UI.input
                            # set style [("width","50%"),("text-align","center")]

    field_card1      <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_card2      <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_card3      <- UI.input
                           # set style [("width","20%"),("text-align","center")]


 {-----------------------------------------------------------------------------
     D. Hoverhoff
 ------------------------------------------------------------------------------}

    text_field_domain       <- UI.span # set text "\\(|D|:\\)"
    on UI.hover text_field_domain $ \_ -> do
        element text_field_domain # set text "Give me a domain size!"
    on UI.leave text_field_domain $ \_ -> do
        element text_field_domain # set text "\\(|D|:\\)"

    text_field_k            <- UI.span # set text "\\(k:\\)"
    on UI.hover text_field_k $ \_ -> do
        element text_field_k # set text "Give me some non-negative integer"
    on UI.leave text_field_k $ \_ -> do
        element text_field_k # set text "\\(k:\\)"

    text_field_m            <- UI.span # set text "\\(m:\\)"
    on UI.hover text_field_m $ \_ -> do
        element text_field_m # set text "Give me some non-negative integer"
    on UI.leave text_field_m $ \_ -> do
        element text_field_m # set text "\\(m:\\)"

    text_field_samplesize   <- UI.span # set text "Sample size:"
    on UI.hover text_field_samplesize $ \_ -> do
        element text_field_samplesize # set text "Darf's a bissal mehr sein?"
    on UI.leave text_field_samplesize $ \_ -> do
        element text_field_samplesize # set text "Sample size:"

    text_field_card1        <- UI.span # set text "\\( | S \\cap R | :\\)"
    on UI.hover text_field_card1  $ \_ -> do
        element text_field_card1  # set text "Size of intersection of scope and range"
    on UI.leave text_field_card1  $ \_ -> do
        element text_field_card1  # set text "\\( | S \\cap R | :\\)"

    text_field_card2        <- UI.span # set text "\\( | S \\setminus R | :\\)"
    on UI.hover text_field_card2 $ \_ -> do
        element text_field_card2 # set text "Size of scope without range"
    on UI.leave text_field_card2 $ \_ -> do
        element text_field_card2 # set text "\\( | S \\setminus R | :\\)"

    text_field_card3        <- UI.span # set text "\\( | R \\setminus S | :\\)"
    on UI.hover text_field_card3  $ \_ -> do
        element text_field_card3  # set text "Size of scope without range"
    on UI.leave text_field_card3  $ \_ -> do
        element text_field_card3  # set text "\\( | R \\setminus S | :\\)"

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}

    getBody window #+ [element mathjaxScript1,
                       element mathjaxScript2,
                       --UI.p # set text "\\[\\Pi^k_m x . S(x)\\]",
                       UI.p # set text "      Semi-Fuzzy Playground" # set style [("color", "blue"), ("text-align", "center")],
                       row [
                            column [
                                   grid [
                                           [element wr_input, element wor_input]
                                   ] # set style [
                                          ("padding-left", "47px"), ("padding-top", "7px")
                                          ],
                                   grid [
                                           [element text_field_k , element field_k],
                                           [element text_field_m , element field_m]
                                        ] # set style [
                                          ("text-align", "center"),
                                          ("padding-top", "22px"),
                                          ("padding-left", "35px"),
                                          ("padding-bottom", "5px")
                                          ]
                                   ],
                            column [
                                   grid [
                                           [element text_field_domain, element field_domain],
                                           [element text_field_card1, element field_card1],
                                           [element text_field_card2, element field_card2],
                                           [element text_field_card3, element field_card3]
                                        ]  # set style [
                                          ("text-align", "center"),
                                          ("padding-left", "35px"),
                                          ("padding-bottom", "5px"),
                                          ("padding-top", "5px")
                                          ]
                                   ]
                            ]# set style [
                              ("background","rgba(11, 127, 171,0.4)"),
                              ("padding-bottom", "5px")
                              ] ,
                            UI.hr # set style [("border-top", "1px solid darkblue")],
                            row [element text_field_samplesize, element field_samplesize] # set style [
                                  ("padding-top", "5px"),
                                  ("padding-left", "45px")
                                ],
                            row [element button] # set style [
                                  ("padding-left", "137px"),
                                  ("padding-top", "15px")
                                ],
                            row [UI.span # set text "Formula approximately evaluates to: ", element resultApp] # set style [
                                  ("text-align", "right"), ("padding-top", "15px"), ("color", "blue")],
                            row [UI.span # set text "Formula evaluates to: ", element result ] # set style [
                                  ("text-align", "right"), ("padding-top", "5px"), ("color", "blue")]
                       ] # set style [
                           ("background-color","lightskyblue"),
                           ("text-align", "right"),
                           ("color","black"),
                           ("font-size","10px"),
                           ("height", "200px"),
                           ("width","370px"),
                           ("border", "2px solid blue"),
                           ("padding-bottom", "61px")
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
