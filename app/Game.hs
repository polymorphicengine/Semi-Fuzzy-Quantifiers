module Game where

import qualified Data.Map as Map
import Data.List
import System.Random
import Control.Monad

import Data.Maybe

data Mode = WR | WOR | BC_L | BC_G | BC_H | BC_Rule deriving (Eq,Show)

type Elem = Int

data Formula = Bot | P Elem | Q {modifier :: Int
                                ,mode :: Mode
                                ,param1 :: Int
                                ,param2 :: Int
                                } deriving Eq

data Interpretation = I {rangeI :: [Elem]
                        ,scopeI :: [Elem]
                        } deriving (Eq, Show)

-- the domain for an integer n is actually [0..n-1]
newtype Domain = Dom Int deriving (Eq, Show)

data GameState = GS [Formula] [Formula] | Stop deriving (Eq, Show)

-- prettier show instances

instance Show Formula where
  show (P p) = show p
  show (Q _ mode k m) = show mode ++ "^" ++ show k ++ "_" ++ show m
  show Bot = "Bot"


atomRisk :: Interpretation -> Formula -> Double
atomRisk (I _ ss) (P e) = if elem e ss then 0 else 1
atomRisk _ Bot = 1
atomRisk _ _ = error "Cannot evaluate a gamestate that is not fully expanded"


drawOnce :: Domain -> Interpretation -> IO Int
drawOnce d@(Dom n) (I [] _) = error "The range predicate should not be falsum"
drawOnce d@(Dom n) i@(I rs _) = do
              x <- getStdRandom (randomR (0,n-1))
              case elem x rs of
                True -> return x
                False -> drawOnce d i

drawWR :: Domain -> Interpretation -> Int -> IO [Int]
drawWR d i n = replicateM n $ drawOnce d i

drawWOR :: Domain -> Interpretation -> Int -> [Int] -> IO [Int]
drawWOR d i 0 is = return []
drawWOR d@(Dom k) i n is = case n > k of
                              True -> error "Can't choose that many constants without repetition"
                              False -> do
                                    x <- drawOnce d i
                                    case elem x is of
                                      True -> drawWOR d i n is
                                      False -> do
                                            xs <- drawWOR d i (n-1) (x:is)
                                            return (x:xs)

genSubsetsK :: Int -> [Int] -> [[Int]]
genSubsetsK 0 is = [[]]
genSubsetsK _ [] = []
genSubsetsK k (i:is) = [i:xs | xs <- (genSubsetsK (k-1) is)] ++ (genSubsetsK k is)

genSubsetsKM' :: [Int] -> [[Int]] -> [([Int],[Int])]
genSubsetsKM' cs lss = [(diff cs ls, ls) | ls <- lss]
                    where diff cs [] = cs
                          diff [] _ = []
                          diff (c:cs) (l:ls) = if c == l then diff cs ls else c:(diff cs (l:ls))


genSubsetsKM ::  Int -> [Int] -> [([Int],[Int])]
genSubsetsKM k is = genSubsetsKM' is (genSubsetsK k is)

substitute :: [Elem] -> [Formula]
substitute = map P

expandFormula :: Domain -> Interpretation -> Formula -> IO [GameState]
expandFormula _ _ Bot = error "Can't expand bot"
expandFormula _ _ (P _) = error "Can't be called on atoms"
expandFormula d i (Q _ mode k m) = case mode of
                                        WR -> do
                                            is <- drawWR d i (k+m)
                                            let subs = genSubsetsKM k is
                                                gameStates = map (\(yours,mys) -> GS (substitute yours) (substitute mys ++ replicate m Bot)) subs
                                            return $ gameStates ++ [Stop]
                                        WOR -> do
                                            is <- drawWOR d i m []
                                            let subs = [is | (_,is) <- genSubsetsKM k is]
                                                gameStates = map (\sub -> GS [] (substitute sub)) subs
                                            return $ gameStates ++ [Stop]
                                        BC_L -> do
                                          us <- drawWR d i (k+m)
                                          let yourFormulas = substitute us
                                              gameState = GS yourFormulas (replicate m Bot)
                                              val = - (fromIntegral k) + (fromIntegral $ m+k)*(proportion i)
                                          return $ if val <= 0 then [GS [] []] else if val <= 1 then [gameState] else [Stop]
                                        BC_G -> do
                                          us <- drawWR d i (k+m)
                                          let myFormulas = substitute us
                                              gameState = GS (replicate m Bot) myFormulas
                                              val = (fromIntegral k) - (fromIntegral $ m+k)*(proportion i)
                                          return $ if val <= 0 then [GS [] []] else if val <= 1 then [gameState] else [Stop]
                                        BC_H -> do
                                          let risk1 = blindChoiceRisk (2*k) 0 0 (k-m) (proportion i)
                                              risk2 = blindChoiceRisk 0 (k+m) (2*k) 0 (proportion i)
                                          cs <- drawWR d i (2*k)
                                          case risk1 <= 0 of
                                            True -> case risk2 <= 0 of
                                                        True -> return [GS [] []]
                                                        False -> return $ if risk2 <= 1 then [GS (replicate (k+m) Bot ) (substitute cs)] else [Stop]
                                            False -> case risk1 <= risk2 of
                                                        True -> return $ if risk2 <= 1 then [GS (replicate (k+m) Bot ) (substitute cs)] else [Stop]
                                                        False -> return $ if risk1 <= 1 then [GS (substitute cs) (replicate (k-m) Bot )] else [Stop]


riskGS :: Interpretation -> GameState -> Double
riskGS i g@(GS us is) = myRisk - yourRisk
                       where myRisk = sum $ map (\p -> atomRisk i p) is
                             yourRisk = sum $ map (\p -> atomRisk i p) us
riskGS _ Stop = 1 -- limited liability


play :: Formula -> Domain -> Interpretation -> IO Double
play f d i = expandFormula d i f >>= \gss -> return $ (minimum $ map (\gs -> riskGS i gs) gss)

playMany :: Int -> Formula -> Domain -> Interpretation -> IO Double
playMany samplesize f d i = do
                        v <- fmap sum $ replicateM samplesize (play f d i)
                        return $ 1 - v / (fromIntegral samplesize)

playWGeneric :: Double -> Formula -> Domain -> Interpretation -> IO Double
playWGeneric value f@(Q n mode k m) d i | n == 0 = play f d i
                                        | otherwise = case (fromIntegral $ n+1)*(1 - value) - (fromIntegral n) <= 0 of
                                                      True -> return 0
                                                      False -> do
                                                           gameStatess <- sequence $ map (\f -> expandFormula d i f) (replicate (n+1) (Q 0 mode k m))
                                                           let gameRisks = map (\gss -> minimum $ map (\gs -> riskGS i gs) gss) gameStatess
                                                           return $ sum gameRisks - (fromIntegral n)

playW :: Formula -> Domain -> Interpretation -> IO Double
playW f@(Q n WR k m) d i = playWGeneric (valWR (fromIntegral k) (fromIntegral m) (proportion i)) f d i
playW f@(Q n WOR k m) d@(Dom size) i = playWGeneric (valWOR (fromIntegral size) (fromIntegral k) (fromIntegral m) (proportion i)) f d i
playW f@(Q n BC_L k m) d i = playWGeneric (valBC_L (fromIntegral k) (fromIntegral m) (proportion i)) f d i
playW f@(Q n BC_G k m) d i = playWGeneric (valBC_G (fromIntegral k) (fromIntegral m) (proportion i)) f d i
playW f@(Q n BC_H k m) d i = playWGeneric (valBC_H (fromIntegral k) (fromIntegral m) (proportion i)) f d i

playWMany :: Int -> Formula -> Domain -> Interpretation -> IO Double
playWMany samplesize f d i = do
                        v <- fmap sum $ replicateM samplesize (playW f d i)
                        return $ 1 - v / (fromIntegral samplesize)
--

proportion :: Interpretation -> Double
proportion (I rs ss) = (fromIntegral $ length int) / (fromIntegral $ length rs)
                     where int = [x | x <- rs, elem x ss]

--

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial $ n-1)

binom :: Double -> Integer -> Double
binom r k | k >= 0 =  product [(r - (fromInteger j) + 1)/(fromInteger j) | j <- [1..k]]
          | otherwise = error "k < 0"

valWR :: Integer -> Integer -> Double -> Double
valWR n k r = (binom (fromInteger $ n+k) n)*r^n*(1-r)^k

valWOR :: Integer -> Integer -> Integer -> Double -> Double
valWOR d j k prop = (sum [ smd i | i <- [j..k]] ) / (binom (fromInteger d) k)
                  where smd i = ((binom ((fromInteger d)*prop) i) * (binom ((fromInteger d)*(1-prop)) (k-i)))

valBC_L :: Integer -> Integer -> Double -> Double
valBC_L k m prop = min 1 (max 0 (1+(fromIntegral k)-(fromIntegral $ m+k)*prop))

valBC_G :: Integer -> Integer -> Double -> Double
valBC_G k m prop = min 1 (max 0 (1-(fromIntegral k)+(fromIntegral $ m+k)*prop))

valBC_H :: Integer -> Integer -> Double -> Double
valBC_H k m prop = min exValG exValL
                where exValG = valBC_G (fromIntegral $ k-m) (fromIntegral $ k + m) prop
                      exValL = valBC_L (fromIntegral $ k+m) (fromIntegral $ k - m) prop


--

exValW :: Double -> Int -> Double
exValW val n = 1 - (max 0 ((1-val)*(fromIntegral $ n+1) - (fromIntegral $ n)))

blindChoiceRisk :: Int -> Int -> Int -> Int -> Double -> Double
blindChoiceRisk r s u v prop = (fromIntegral v) - (fromIntegral s) + (fromIntegral $ u-r)*(1-prop)



-----


data Defense = D {yourFs :: Int
                 ,yourBots :: Int
                 ,myFs :: Int
                 ,myBots :: Int
                 } deriving (Eq,Show)

type Attack = [Defense]

type Rule = [Attack]

defenseRisk :: Defense -> Double -> Double
defenseRisk (D r s u v) prop = (fromIntegral v) - (fromIntegral s) + (fromIntegral $ u-r)*(1-prop)

chooseAttack :: Rule -> Interpretation -> Defense
chooseAttack rule i = [d | d <- defenses, defenseRisk d prop == maximum risks]!!0
                    where defenses = map (\a -> chooseDefense a i) rule
                          risks = map (\d -> defenseRisk d prop) defenses
                          prop = proportion i

chooseDefense :: Attack -> Interpretation -> Defense
chooseDefense att i = [d | d <- att, defenseRisk d prop == minimum risks]!!0
                     where risks = map (\d -> defenseRisk d prop) att
                           prop = proportion i

toGameState :: Defense -> [Formula] -> [Formula] -> GameState
toGameState (D _ s _ v) yourFs myFs = GS (yourFs ++ replicate s Bot) (myFs ++ replicate v Bot)

genericBC :: Defense -> Domain -> Interpretation -> IO GameState
genericBC def d i = do
                let myAmount = myFs def
                    yourAmount = yourFs def
                is <- drawWR d i myAmount
                us <- drawWR d i yourAmount
                let myForms = substitute is
                    yourForms = substitute us
                return $ toGameState def yourForms myForms

playBC :: Defense -> Domain -> Interpretation -> IO Double
playBC def d i = do
                gs <- genericBC def d i
                return $ riskGS i gs

playBCMany :: Int -> Rule -> Domain -> Interpretation -> IO Double
playBCMany n r d i = do
                let def = chooseAttack r i
                v <- fmap sum $ replicateM n (playBC def d i)
                return $ 1 - v / (fromIntegral n)

--

ruleValue :: Rule -> Double -> Double
ruleValue r prop = minimum $ map (flip attackValue prop) r

attackValue :: Attack -> Double -> Double
attackValue a prop = maximum $ map (flip defenseValue prop) a

defenseValue :: Defense -> Double -> Double
defenseValue d prop = 1 - (defenseRisk d prop)

--

playWBC :: Double -> Int -> Defense -> Domain -> Interpretation -> IO Double
playWBC value n def d i | n == 0 = playBC def d i
                             | otherwise = case (fromIntegral $ n+1)*(1 - value) - (fromIntegral n) <= 0 of
                                                      True -> return 0
                                                      False -> do
                                                           myValue <- fmap sum $ replicateM (n+1) (playBC def d i)
                                                           return $ myValue - (fromIntegral n)


playWBCMany :: Int -> Int -> Rule -> Domain -> Interpretation -> IO Double
playWBCMany samplesize n rule d i = do
                let def = chooseAttack rule i
                v <- fmap sum $ replicateM samplesize (playWBC value n def d i)
                return $ 1 - v / (fromIntegral samplesize)
                where value = ruleValue rule (proportion i)
