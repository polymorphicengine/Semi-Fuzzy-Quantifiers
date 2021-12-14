module GUI where

import Control.Monad (void, replicateM, zipWithM)
import Data.Maybe
import Data.IORef
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

{-----------------------------------------------------------------------------
  Scripts
------------------------------------------------------------------------------}

    mathjaxScript1 <- (UI.mkElement "script")
                # set UI.src "https://polyfill.io/v3/polyfill.min.js?features=es6"
    mathjaxScript2 <- (UI.mkElement "script")
                # set UI.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"

{-----------------------------------------------------------------------------
  Checkboxes
------------------------------------------------------------------------------}

    wr_check <- UI.input # set UI.type_ "checkbox"
    wor_check <- UI.input # set UI.type_ "checkbox"
    bcL_check <- UI.input # set UI.type_ "checkbox"
    bcG_check <- UI.input # set UI.type_ "checkbox"
    bcH_check <- UI.input # set UI.type_ "checkbox"
    w_check <- UI.input # set UI.type_ "checkbox"
    rule_check <- UI.input # set UI.type_ "checkbox"

    wr_input <- UI.label #+ [element wr_check,
                              UI.span # set text "\\( \\Pi^k_m \\)"
                             ]
    wor_input <- UI.label #+ [element wor_check,
                             UI.span # set text "\\( \\Pi^{j,k} \\)"
                            ]
    bcL_input <- UI.label #+ [element bcL_check,
                              UI.span # set text "\\( L^k_m \\)"
                             ]
    bcG_input <- UI.label #+ [element bcG_check,
                             UI.span # set text "\\( G^k_m \\)"
                            ]
    bcH_input <- UI.label #+ [element bcH_check,
                             UI.span # set text "\\( H^s_t \\)"
                            ]
    w_input <- UI.label #+ [element w_check,
                             UI.span # set text "\\( W_n \\)"
                            ]
    rule_input <- UI.label #+ [element rule_check,
                             UI.span # set text "\\( R \\)"
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
    field_n          <- UI.input
                            # set style [("width","20%"), ("text-align", "center")]
    field_samplesize <- UI.input
                            # set style [("width","50%"),("text-align","center")]
    field_scopeSize  <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_intSize    <- UI.input
                           # set style [("width","20%"),("text-align","center")]
    field_rangeSize  <- UI.input
                           # set style [("width","20%"),("text-align","center")]


 {-----------------------------------------------------------------------------
     Textfields
 ------------------------------------------------------------------------------}

    text_field_domain       <- UI.span # set text "\\(|D|:\\)"

    text_field_k            <- UI.span # set text "\\(k:\\)"
    -- tooltip_k <- UI.span #. "tooltiptext" # set text "Enter a non-negative integer"

    text_field_m            <- UI.span # set text "\\(m:\\)"
    -- tooltip_m <- UI.span #. "tooltiptext" # set text "Enter a non-negative integer"

    text_field_n            <- UI.span # set text "\\(n:\\)"

    text_field_samplesize   <- UI.span # set text "Sample size:"

    text_field_scope        <- UI.span # set text "\\( | S | :\\)"

    text_field_int          <- UI.span # set text "\\( | R \\cap S | :\\)"

    text_field_range        <- UI.span # set text "\\( | R | :\\)"


    (rule,ref) <- mkRule
{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}

    getBody window #+ [element mathjaxScript1,
                       element mathjaxScript2,
                       UI.p # set text "      Semi-Fuzzy Playground" # set style [("color", "blue"), ("text-align", "center")],
                       row [
                            column [
                                   grid [
                                           [element wr_input, element wor_input, element rule_input],
                                           [element bcL_input, element bcG_input, element bcH_input],
                                           [element w_input]
                                   ] # set style [
                                          ("padding-left", "47px"), ("padding-top", "7px")
                                          ],
                                   grid [
                                           [element text_field_k, element field_k],
                                           [element text_field_m, element field_m],
                                           [element text_field_n, element field_n]
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
                            ] # set style [
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
                                  ("text-align", "center"), ("padding-top", "15px"), ("color", "blue")],
                            row [rule]
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
                       n' <- field_n # get UI.value
                       samplesize' <- field_samplesize # get UI.value
                       scopeSize' <- field_scopeSize # get UI.value
                       intSize' <- field_intSize # get UI.value
                       rangeSize' <- field_rangeSize # get UI.value

                       modeWR <- wr_check # get UI.checked
                       modeWOR <- wor_check # get UI.checked
                       modeBCL <- bcL_check # get UI.checked
                       modeBCG <- bcG_check # get UI.checked
                       modeBCH <- bcH_check # get UI.checked
                       modifier <- w_check # get UI.checked
                       modeRule <- rule_check # get UI.checked

                       let mode = case (modeWR,modeWOR,modeBCL,modeBCG,modeBCH,modeRule) of
                                          (True,False,False,False,False,False) -> WR
                                          (False,True,False,False,False,False) -> WOR
                                          (False,False,True,False,False,False) -> BC_L
                                          (False,False,False,True,False,False) -> BC_G
                                          (False,False,False,False,True,False) -> BC_H
                                          (False,False,False,False,False,True) -> BC_Rule
                                          _ -> WR

                       let may_domain_size = readMaybe domain_size'
                           may_k = readMaybe k'
                           may_m = readMaybe m'
                           may_n = readMaybe n'
                           may_samplesize = readMaybe samplesize'
                           may_scopeSize = readMaybe scopeSize'
                           may_intSize = readMaybe intSize'
                           may_rangeSize = readMaybe rangeSize'

                       case elem Nothing [may_domain_size, may_k, may_m, may_samplesize, may_scopeSize, may_intSize, may_rangeSize, may_n] of
                         True -> element display # set UI.text "Please only enter non negative integers"
                         False -> do
                           let domain_size = strip may_domain_size
                               k = strip may_k
                               m = strip may_m
                               n = strip may_n
                               samplesize = strip may_samplesize
                               scopeSize = strip may_scopeSize
                               intSize = strip may_intSize
                               rangeSize = strip may_rangeSize
                               card1 = intSize
                               card2 = scopeSize - intSize
                               card3 = rangeSize - intSize
                               prop = (fromIntegral card1) / (fromIntegral (card1+card3))
                               interpScope = [x | x <- [0..card1 - 1]] ++ [x | x <- [card1 .. (card1 + card2) - 1]]
                               interpRange = [x | x <- [0..card1 - 1]] ++ [x | x <- [(card1 + card2) .. (card1 + card2 + card3) - 1]]
                               interpretation = I interpRange interpScope

                           case all (>= 0) [domain_size,k,m,samplesize,card1,card2,card3] of
                                False -> element display # set UI.text "Please only enter non negative integers"
                                True -> case card1 + card2 + card3 <= domain_size of
                                    False -> element display # set UI.text "The sum of the cardinalities exceeds the size of the domain!"
                                    True -> case card1 + card3 == 0 of
                                                True -> element display # set UI.text "Range should not be falsum!"
                                                False -> case (mode, modifier) of
                                                             (WR, False) -> do
                                                                let formula = Q 0 WR k m
                                                                v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                    exVal = (show $ valWR (fromIntegral k) (fromIntegral m) prop)
                                                                element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)

                                                             (WOR, False) -> case k+m <= domain_size of
                                                                          False -> element display # set UI.text "Can't select that many elements without repetition!"
                                                                          True -> do
                                                                              let formula = Q 0 WOR k m
                                                                              v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                              let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                                  exVal = (show $ valWOR (fromIntegral domain_size) (fromIntegral k) (fromIntegral m) prop)
                                                                              element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)
                                                             (BC_L, False) -> do
                                                               let formula = Q 0 BC_L k m
                                                               v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                               let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                   exVal = (show $ valBC_L (fromIntegral k) (fromIntegral m) prop)
                                                               element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)
                                                             (BC_G, False) -> do
                                                                let formula = Q 0 BC_G k m
                                                                v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                    exVal = (show $ valBC_G (fromIntegral k) (fromIntegral m) prop)
                                                                element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)
                                                             (BC_H, False) -> do
                                                                 let formula = Q 0 BC_H k m
                                                                 v <- liftIO $ fmap sum $ replicateM samplesize (play formula (Dom domain_size) interpretation)
                                                                 let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                     exVal = (show $ valBC_H (fromIntegral k) (fromIntegral m) prop)
                                                                 element display # set UI.text ("The approximated value is: " ++ appVal ++ "\n The exact value is: " ++ exVal)
                                                             (WR, True) -> do
                                                                let formula = Q n WR k m
                                                                v <- liftIO $ fmap sum $ replicateM samplesize (playW formula (Dom domain_size) interpretation)
                                                                let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                    exVal = show $ exValW (valWR (fromIntegral k) (fromIntegral m) prop) n
                                                                element display # set UI.text ("The approximated value is: " ++ appVal  ++ "\n The exact value is: " ++ exVal)
                                                             (WOR, True) -> do
                                                                   let formula = Q n WOR k m
                                                                   v <- liftIO $ fmap sum $ replicateM samplesize (playW formula (Dom domain_size) interpretation)
                                                                   let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                       exVal = show $ exValW (valWOR (fromIntegral domain_size) (fromIntegral k) (fromIntegral m) prop) n
                                                                   element display # set UI.text ("The approximated value is: " ++ appVal  ++ "\n The exact value is: " ++ exVal)
                                                             (BC_L, True) -> do
                                                                   let formula = Q n BC_L k m
                                                                   v <- liftIO $ fmap sum $ replicateM samplesize (playW formula (Dom domain_size) interpretation)
                                                                   let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                       exVal = show $ exValW (valBC_L (fromIntegral k) (fromIntegral m) prop) n
                                                                   element display # set UI.text ("The approximated value is: " ++ appVal  ++ "\n The exact value is: " ++ exVal)
                                                             (BC_G, True) -> do
                                                                   let formula = Q n BC_G k m
                                                                   v <- liftIO $ fmap sum $ replicateM samplesize (playW formula (Dom domain_size) interpretation)
                                                                   let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                       exVal = show $ exValW (valBC_G (fromIntegral k) (fromIntegral m) prop) n
                                                                   element display # set UI.text ("The approximated value is: " ++ appVal  ++ "\n The exact value is: " ++ exVal)
                                                             (BC_H, True) -> do
                                                                   let formula = Q n BC_H k m
                                                                   v <- liftIO $ fmap sum $ replicateM samplesize (playW formula (Dom domain_size) interpretation)
                                                                   let appVal = (show $ 1 - v / (fromIntegral samplesize))
                                                                       exVal = show $ exValW (valBC_H (fromIntegral k) (fromIntegral m) prop) n
                                                                   element display # set UI.text ("The approximated value is: " ++ appVal  ++ "\n The exact value is: " ++ exVal)
                                                             (BC_Rule, False) -> do
                                                                   rule <- getRule ref
                                                                   v <- liftIO $ playBCMany samplesize rule (Dom domain_size) interpretation
                                                                   let exVal = show $ ruleValue rule prop
                                                                   element display # set UI.text ("The approximated value is: " ++ show v ++ "\n The exact value is: " ++ exVal)
                                                             (BC_Rule, True) -> do
                                                                   rule <- getRule ref
                                                                   v <- liftIO $ playWBCMany samplesize n rule (Dom domain_size) interpretation
                                                                   let exVal = show $ exValW (ruleValue rule prop) n
                                                                   element display # set UI.text ("The approximated value is: " ++ show v ++ "\n The exact value is: " ++ exVal)
    on UI.click button $ const $ startGameFunction

    on UI.checkedChange wr_check $ const $ do
                                element wor_check # set UI.checked False
                                element bcL_check # set UI.checked False
                                element bcG_check # set UI.checked False
                                element bcH_check # set UI.checked False
                                element rule_check # set UI.checked False
                                element text_field_k # set text "\\(k:\\)"
                                element text_field_m # set text "\\(m:\\)"
                                typeset

    on UI.checkedChange wor_check $ const $ do
                                element wr_check # set UI.checked False
                                element bcL_check # set UI.checked False
                                element bcG_check # set UI.checked False
                                element bcH_check # set UI.checked False
                                element rule_check # set UI.checked False
                                element text_field_k # set text "\\(j:\\)"
                                element text_field_m # set text "\\(k:\\)"
                                typeset

    on UI.checkedChange bcG_check $ const $ do
                                element wor_check # set UI.checked False
                                element bcL_check # set UI.checked False
                                element wr_check # set UI.checked False
                                element bcH_check # set UI.checked False
                                element rule_check # set UI.checked False
                                element text_field_k # set text "\\(k:\\)"
                                element text_field_m # set text "\\(m:\\)"
                                typeset

    on UI.checkedChange bcL_check $ const $ do
                                element wr_check # set UI.checked False
                                element wor_check # set UI.checked False
                                element bcG_check # set UI.checked False
                                element bcH_check # set UI.checked False
                                element rule_check # set UI.checked False
                                element text_field_k # set text "\\(k:\\)"
                                element text_field_m # set text "\\(m:\\)"
                                typeset

    on UI.checkedChange bcH_check $ const $ do
                                element wr_check # set UI.checked False
                                element wor_check # set UI.checked False
                                element bcG_check # set UI.checked False
                                element bcL_check # set UI.checked False
                                element rule_check # set UI.checked False
                                element text_field_k # set text "\\(s:\\)"
                                element text_field_m # set text "\\(t:\\)"
                                typeset


{-----------------------------------------------------------------------------
   Functionality
------------------------------------------------------------------------------}

strip :: Maybe a -> a
strip (Just x) = x
strip Nothing = error "Something went wrong"

typeset :: UI ()
typeset = runFunction $ ffi "MathJax.typeset()"

{-----------------------------------------------------------------------------
   Blind Choice Rule Input
------------------------------------------------------------------------------}

mkDefense :: Int -> Int -> UI Element
mkDefense n m = do
            r  <- UI.input # set UI.id_ ("r" ++ show n ++ "," ++ show m) # set style [("width","20%"),("text-align","center")]
            s  <- UI.input # set UI.id_ ("s" ++ show n ++ "," ++ show m) # set style [("width","20%"),("text-align","center")]
            u  <- UI.input # set UI.id_ ("u" ++ show n ++ "," ++ show m) # set style [("width","20%"),("text-align","center")]
            v  <- UI.input # set UI.id_ ("v" ++ show n ++ "," ++ show m) # set style [("width","20%"),("text-align","center")]
            row [UI.label #+ [UI.span # set text ("\\(r_{" ++ show n ++ "," ++ show m ++ "} : \\)"), element r]
                ,UI.label #+ [UI.span # set text ("\\(s_{" ++ show n ++ "," ++ show m ++ "} : \\)"), element s]
                ,UI.label #+ [UI.span # set text ("\\(u_{" ++ show n ++ "," ++ show m ++ "} : \\)"), element u]
                ,UI.label #+ [UI.span # set text ("\\(v_{" ++ show n ++ "," ++ show m ++ "} : \\)"), element v]
                ] # set UI.id_ ("defense" ++ show n ++ "," ++ show m)

getDefense :: Int -> Int -> UI Defense
getDefense n m = do
              w <- askWindow
              r' <- fmap strip $ getElementById w ("r" ++ show n ++ "," ++ show m)
              s' <- fmap strip $ getElementById w ("s" ++ show n ++ "," ++ show m)
              u' <- fmap strip $ getElementById w ("u" ++ show n ++ "," ++ show m)
              v' <- fmap strip $ getElementById w ("v" ++ show n ++ "," ++ show m)
              r <- fmap read $ r' # get UI.value
              s <- fmap read $ s' # get UI.value
              u <- fmap read $ u' # get UI.value
              v <- fmap read $ v' # get UI.value
              return $ D r s u v

addDefense :: Int -> Int -> UI ()
addDefense n m = do
            w <- askWindow
            att <- fmap strip $ getElementById w ("attack" ++ show n)
            def <- mkDefense n m
            void $ element att #+ [element def]

mkAttack :: Int -> UI (UI Element , IORef Int)
mkAttack n = do
  plusButton <- UI.button # set UI.text "+"
  ref <- liftIO $ newIORef 1
  on UI.click plusButton $ \_ -> do
        i <- liftIO $ readIORef ref
        addDefense n i
        liftIO $ modifyIORef ref (+1)
        typeset
  return (row [element plusButton, UI.div # set UI.id_ ("attack" ++ show n)], ref)

getAttack :: Int -> IORef Int -> UI Attack
getAttack n ref = do
      m <- liftIO $ readIORef ref
      fmap ((D 0 0 0 1):) $ zipWithM getDefense (replicate (m-1) n) [1..(m-1)]

addAttack :: Int -> UI (IORef Int)
addAttack n = do
        w <- askWindow
        rule <- fmap strip $ getElementById w "rule"
        (att,ref) <- mkAttack n
        element rule #+ [att]
        return ref

mkRule :: UI (UI Element, IORef [IORef Int])
mkRule = do
  plusButton <- UI.button # set UI.text "+"
  attsRef <- liftIO $ newIORef []
  on UI.click plusButton $ \_ -> do
        atts <- liftIO $ readIORef attsRef
        let i = length atts + 1
        ref <- addAttack i
        liftIO $ modifyIORef attsRef (\x -> x ++ [ref])
        typeset
  return (row [element plusButton, UI.div # set UI.id_ "rule"], attsRef)


getRule :: IORef [IORef Int] -> UI Rule
getRule ref = do
          refs <- liftIO $ readIORef ref
          fmap ([D 0 0 0 0]:) $ zipWithM getAttack [1..length refs] refs
