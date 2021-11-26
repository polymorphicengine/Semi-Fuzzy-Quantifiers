module GUI where

import Control.Monad (void, replicateM)
import Data.Maybe
import qualified Data.Map as Map
import Text.Printf
--import Safe          (readMay)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import Text.Read (readMaybe)

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

    tooltipContainer <- UI.div #. "tooltip"
    tooltipstyle <- (UI.mkElement "style") # set text ".tooltip {position: relative;display: inline-block;} .tooltip .tooltiptext { visibility: hidden; width: 120px; background-color: #555; color: #fff; text-align: center; padding: 5px 0; border-radius: 6px; position: absolute; z-index: 1; bottom: 125%; left: 50%; margin-left: -60px; opacity: 0; transition: opacity 0.3s;} .tooltip:hover .tooltiptext {visibility: visible;opacity: 1;}"

{-----------------------------------------------------------------------------
    Display fields
------------------------------------------------------------------------------}

    display <- UI.pre

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

    field_scopeSize      <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_intSize      <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_rangeSize      <- UI.input
                           # set style [("width","20%"),("text-align","center")]


 {-----------------------------------------------------------------------------
     D. Hoverhoff
 ------------------------------------------------------------------------------}

    text_field_domain       <- UI.span # set text "\\(|D|:\\)"
    -- on UI.hover text_field_domain $ \_ -> do
    --     element text_field_domain # set text "Give me a domain size!"
    -- on UI.leave text_field_domain $ \_ -> do
    --     element text_field_domain # set text "\\(|D|:\\)"

    text_field_k            <- UI.span # set text "\\(k:\\)"
    -- tooltip_k <- UI.span #. "tooltiptext" # set text "Enter a non-negative integer"

    text_field_m            <- UI.span # set text "\\(m:\\)"
    -- tooltip_m <- UI.span #. "tooltiptext" # set text "Enter a non-negative integer"

    text_field_samplesize   <- UI.span # set text "Sample size:"
    -- on UI.hover text_field_samplesize $ \_ -> do
    --     element text_field_samplesize # set text "Darf's a bissal mehr sein?"
    -- on UI.leave text_field_samplesize $ \_ -> do
    --     element text_field_samplesize # set text "Sample size:"

    text_field_scope        <- UI.span # set text "\\( | S | :\\)"
    -- on UI.hover text_field_card1  $ \_ -> do
    --     element text_field_card1  # set text "Size of intersection of scope and range"
    -- on UI.leave text_field_card1  $ \_ -> do
    --     element text_field_card1  # set text "\\( | S \\cap R | :\\)"

    text_field_int        <- UI.span # set text "\\( | R \\cap S | :\\)"
    -- on UI.hover text_field_card2 $ \_ -> do
    --     element text_field_card2 # set text "Size of scope without range"
    -- on UI.leave text_field_card2 $ \_ -> do
    --     element text_field_card2 # set text "\\( | S \\setminus R | :\\)"

    text_field_range        <- UI.span # set text "\\( | R | :\\)"
    -- on UI.hover text_field_card3  $ \_ -> do
    --     element text_field_card3  # set text "Size of scope without range"
    -- on UI.leave text_field_card3  $ \_ -> do
    --     element text_field_card3  # set text "\\( | R \\setminus S | :\\)"

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}

    getBody window #+ [element mathjaxScript1,
                       element mathjaxScript2,
                       element tooltipstyle,
                       UI.p # set text "      Semi-Fuzzy Playground" # set style [("color", "blue"), ("text-align", "center")],
                       row [
                            column [
                                   grid [
                                           [element wr_input, element wor_input]
                                   ] # set style [
                                          ("padding-left", "47px"), ("padding-top", "7px")
                                          ],
                                   grid [
                                           [element text_field_k, element field_k],
                                           [element text_field_m, element field_m]
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
                                           [element text_field_scope, element field_scopeSize],
                                           [element text_field_int, element field_intSize],
                                           [element text_field_range, element field_rangeSize]
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
                            row [element display] # set style [
                                  ("text-align", "center"), ("padding-top", "15px"), ("color", "blue")]
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
                       scopeSize' <- field_scopeSize # get UI.value
                       intSize' <- field_intSize # get UI.value
                       rangeSize' <- field_rangeSize # get UI.value

                       modeWR <- wr_check # get UI.checked
                       modeWOR <- wor_check # get UI.checked

                       let may_domain_size = readMaybe domain_size'
                           may_k = readMaybe k'
                           may_m = readMaybe m'
                           may_samplesize = readMaybe samplesize'
                           may_scopeSize = readMaybe scopeSize'
                           may_intSize = readMaybe intSize'
                           may_rangeSize = readMaybe rangeSize'

                       case elem Nothing [may_domain_size, may_k, may_m, may_samplesize, may_scopeSize, may_intSize, may_rangeSize] of
                         True -> element display # set UI.text "Please only enter non negative integers"
                         False -> do
                           let domain_size = strip may_domain_size
                               k = strip may_k
                               m = strip may_m
                               samplesize = strip may_samplesize
                               scopeSize = strip may_scopeSize
                               intSize = strip may_intSize
                               rangeSize = strip may_rangeSize
                               card1 = intSize
                               card2 = scopeSize - intSize
                               card3 = rangeSize - intSize
                               interpScope = [[x] | x <- [0..card1 - 1]] ++ [[x] | x <- [card1 .. (card1 + card2) - 1]]
                               interpRange = [[x] | x <- [0..card1 - 1]] ++ [[x] | x <- [(card1 + card2) .. (card1 + card2 + card3) - 1]]
                               interpretation = Map.insert "S" interpScope $ Map.singleton "R" interpRange :: Interpretation

                           case all (>= 0) [domain_size,k,m,samplesize,card1,card2,card3] of
                                False -> element display # set UI.text "Please only enter non negative integers"
                                True -> case card1 + card2 + card3 <= domain_size of
                                    False -> element display # set UI.text "The sum of the cardinalities exceeds the size of the domain!"
                                    True -> case card1 + card3 == 0 of
                                                True -> element display # set UI.text "Range should not be falsum!"
                                                False -> case (modeWR, modeWOR) of
                                                             (True,False) -> do
                                                                          let formula = Quant WR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                                                              prop = (fromIntegral card1) / (fromIntegral (card1+card3)) :: Double
                                                                          v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                          let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                              exVal = (show $ valWR (fromIntegral k) (fromIntegral m) prop)
                                                                          element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)

                                                             (False,True) -> case k+m <= domain_size of
                                                                                False -> element display # set UI.text "Can't select that many elements without repetition!"
                                                                                True -> do
                                                                                    let formula = Quant WOR k m (Var "x") (Pred "R" []) (Pred "S" [V (Var "x")])
                                                                                        prop = (fromIntegral card1) / (fromIntegral (card1+card3)) :: Double
                                                                                    v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                                    let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                                        exVal = (show $ valWOR (fromIntegral domain_size) (fromIntegral k) (fromIntegral m) prop)
                                                                                    element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)

                                                             (False,False) -> element display # set UI.text "Please select a quantifier"
                                                             (True,True) -> element display # set UI.text "Please select a quantifier"

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

strip :: Maybe a -> a
strip (Just x) = x
strip Nothing = error "Something went wrong"
