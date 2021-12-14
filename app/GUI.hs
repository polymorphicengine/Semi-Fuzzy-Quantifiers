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

    wr_check <- mkCheck "wr_check"
    wor_check <- mkCheck "wor_check"
    bcL_check <- mkCheck "bcL_check"
    bcG_check <- mkCheck "bcG_check"
    bcH_check <- mkCheck "bcH_check"
    w_check <- mkCheck "w_check"
    rule_check <- mkCheck "rule_check"

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

{-----------------------------------------------------------------------------
    Display field
------------------------------------------------------------------------------}

    display <- UI.pre # set UI.id_ "display"

{-----------------------------------------------------------------------------
    Start Button
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

    field_domain     <- mkField "domain"
    field_scopeSize  <- mkField "scopeSize"
    field_intSize    <- mkField "intSize"
    field_rangeSize  <- mkField "rangeSize"
    field_samplesize <- mkField "samplesize" # set style [("width","50%"),("text-align","center")]

    field_k          <- mkField "k"
    field_m          <- mkField "m"
    field_n          <- mkField "n"


 {-----------------------------------------------------------------------------
     Textfields
 ------------------------------------------------------------------------------}

    text_field_domain       <- UI.span # set text "\\(|D|:\\)"

    text_field_k            <- UI.span # set text "\\(k:\\)"

    text_field_m            <- UI.span # set text "\\(m:\\)"

    text_field_n            <- UI.span # set text "\\(n:\\)"

    text_field_samplesize   <- UI.span # set text "Sample size:"

    text_field_scope        <- UI.span # set text "\\( | S | :\\)"

    text_field_int          <- UI.span # set text "\\( | R \\cap S | :\\)"

    text_field_range        <- UI.span # set text "\\( | R | :\\)"

{-----------------------------------------------------------------------------
    Rule Input
------------------------------------------------------------------------------}

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

                       vs <- getVals

                       case vs of
                         Nothing -> setText "Please enter non-negative integers"
                         Just (domain_size, scopeSize, intSize, rangeSize, samplesize) -> do

                                         (mode, modifier) <- getMode

                                         let card1 = intSize
                                             card2 = scopeSize - intSize
                                             card3 = rangeSize - intSize
                                             prop = (fromIntegral card1) / (fromIntegral (card1+card3))
                                             interpScope = [x | x <- [0..card1 - 1]] ++ [x | x <- [card1 .. (card1 + card2) - 1]]
                                             interpRange = [x | x <- [0..card1 - 1]] ++ [x | x <- [(card1 + card2) .. (card1 + card2 + card3) - 1]]
                                             interpretation = I interpRange interpScope

                                         case all (>= 0) [domain_size,samplesize,card1,card2,card3] of
                                              False -> element display # set UI.text "Please only enter non negative integers"
                                              True -> case card1 + card2 + card3 <= domain_size of
                                                  False -> element display # set UI.text "The sum of the cardinalities exceeds the size of the domain!"
                                                  True -> case card1 + card3 == 0 of
                                                              True -> element display # set UI.text "Range should not be falsum!"
                                                              False -> case (mode, modifier) of
                                                                           (BC_Rule, False) -> do
                                                                                 rule <- getRule ref
                                                                                 v <- liftIO $ playBCMany samplesize rule (Dom domain_size) interpretation
                                                                                 let exVal = show $ ruleValue rule prop
                                                                                 element display # set UI.text ("The approximated value is: " ++ show v ++ "\n The exact value is: " ++ exVal)
                                                                           (BC_Rule, True) -> do
                                                                                 n' <- getVal "n"
                                                                                 case n' == Nothing of
                                                                                   True -> setText "Please enter non-negative integers"
                                                                                   False -> do
                                                                                       rule <- getRule ref
                                                                                       let n = strip n'
                                                                                           exVal = show $ exValW (ruleValue rule prop) n
                                                                                       v <- liftIO $ playWBCMany samplesize n rule (Dom domain_size) interpretation
                                                                                       element display # set UI.text ("The approximated value is: " ++ show v ++ "\n The exact value is: " ++ exVal)
                                                                           (mode, False) -> genericPlay mode samplesize domain_size interpretation
                                                                           (mode, True) -> genericPlayW mode samplesize domain_size interpretation
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

    on UI.checkedChange rule_check $ const $ do
                                element wr_check # set UI.checked False
                                element wor_check # set UI.checked False
                                element bcG_check # set UI.checked False
                                element bcL_check # set UI.checked False
                                element bcH_check # set UI.checked False
                                typeset

{-----------------------------------------------------------------------------
   Functionality
------------------------------------------------------------------------------}

strip :: Maybe a -> a
strip (Just x) = x
strip Nothing = error "Something went wrong"

typeset :: UI ()
typeset = runFunction $ ffi "MathJax.typeset()"

setText :: String -> UI Element
setText t = do
        w <- askWindow
        disp <- fmap strip $ getElementById w "display"
        element disp # set UI.text t

getVal :: String -> UI (Maybe Int)
getVal idd = do
      w <- askWindow
      ele <- fmap strip $ getElementById w idd
      val <- ele # get UI.value
      return $ readMaybe val

getVals :: UI (Maybe (Int,Int,Int,Int,Int))
getVals = do
  d <- getVal "domain"
  scope <- getVal "scopeSize"
  inter <- getVal "intSize"
  range <- getVal "rangeSize"
  s <- getVal "samplesize"
  case elem Nothing [d,scope,inter,range,s] of
    True -> return Nothing
    False -> return $ Just (strip d, strip scope, strip inter, strip range, strip s)

getCheck :: String -> UI Bool
getCheck idd = do
      w <- askWindow
      ele <- fmap strip $ getElementById w idd
      ele # get UI.checked

getMode :: UI (Mode, Bool)
getMode = do
  modeWR <- getCheck "wr_check"
  modeWOR <- getCheck "wor_check"
  modeBCL <- getCheck "bcL_check"
  modeBCG <- getCheck "bcG_check"
  modeBCH <- getCheck "bcH_check"
  modifier <- getCheck "w_check"
  modeRule <- getCheck "rule_check"

  case (modeWR,modeWOR,modeBCL,modeBCG,modeBCH,modeRule) of
                     (True,False,False,False,False,False) -> return (WR, modifier)
                     (False,True,False,False,False,False) -> return (WOR, modifier)
                     (False,False,True,False,False,False) -> return (BC_L, modifier)
                     (False,False,False,True,False,False) -> return (BC_G, modifier)
                     (False,False,False,False,True,False) -> return (BC_H, modifier)
                     (False,False,False,False,False,True) -> return (BC_Rule, modifier)
                     _ -> setText "Please select a mode" >> return (WR, False)

mkCheck :: String -> UI Element
mkCheck idd = UI.input # set UI.type_ "checkbox" # set UI.id_ idd

mkField :: String -> UI Element
mkField idd = UI.input # set style [("width","20%"),("text-align","center")] # set UI.id_ idd

genericExactValue :: Mode -> Domain -> Int -> Int -> Double -> Double
genericExactValue WR _ k m prop = valWR (fromIntegral k) (fromIntegral m) prop
genericExactValue WOR (Dom x) k m prop = valWOR (fromIntegral x) (fromIntegral k) (fromIntegral m) prop
genericExactValue BC_L _ k m prop = valBC_L (fromIntegral k) (fromIntegral m) prop
genericExactValue BC_G _ k m prop = valBC_G (fromIntegral k) (fromIntegral m) prop
genericExactValue BC_H _ k m prop = valBC_H (fromIntegral k) (fromIntegral m) prop

genericExactValueW :: Mode -> Int -> Domain -> Int -> Int -> Double -> Double
genericExactValueW mode n d k m prop = exValW (genericExactValue mode d k m prop) n

genericPlay :: Mode -> Int -> Int  -> Interpretation -> UI Element
genericPlay mode samplesize domain_size i = do
                            k' <- getVal "k"
                            m' <- getVal "m"
                            case elem Nothing [k',m'] of
                                True -> setText "Please insert non-negative numbers"
                                False -> do
                                    let k = strip k'
                                        m = strip m'
                                        formula = Q 0 mode k m
                                        d = Dom domain_size
                                    appVal <- liftIO $ playMany samplesize formula d i
                                    exVal <- return $ genericExactValue mode d (fromIntegral k) (fromIntegral m) (proportion i)
                                    setText ("The approximated value is: " ++ show appVal ++ "\n The exact value is: " ++ show exVal)

genericPlayW :: Mode -> Int -> Int  -> Interpretation -> UI Element
genericPlayW mode samplesize domain_size i = do
                            k' <- getVal "k"
                            m' <- getVal "m"
                            n' <- getVal "n"
                            case elem Nothing [k',m',n'] of
                                True -> setText "Please insert non-negative numbers"
                                False -> do
                                    let k = strip k'
                                        m = strip m'
                                        n = strip n'
                                        formula = Q n mode k m
                                        d = Dom domain_size
                                    appVal <- liftIO $ playWMany samplesize formula d i
                                    exVal <- return $ genericExactValueW mode n d (fromIntegral k) (fromIntegral m) (proportion i)
                                    setText ("The approximated value is: " ++ show appVal ++ "\n The exact value is: " ++ show exVal)

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
