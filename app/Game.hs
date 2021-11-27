module Game where

import qualified Data.Map as Map
import Data.List
import System.Random
import Control.Monad

import Data.Maybe

data Mode = WR | WOR | BC_L | BC_G deriving (Eq,Show)

type Elem = Int

data Formula = Bot | P Elem | Q {mode :: Mode
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
  show (Q mode k m) = show mode ++ "^" ++ show k ++ "_" ++ show m
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
expandFormula d i (Q mode k m) = case mode of
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


riskGS :: Interpretation -> GameState -> Double
riskGS i g@(GS us is) = myVal - yourVal
                       where myVal = sum $ map (\p -> atomRisk i p) is
                             yourVal = sum $ map (\p -> atomRisk i p) us
riskGS _ Stop = 1 -- limited liability


play :: Formula -> Domain -> Interpretation -> IO Double
play f d i = expandFormula d i f >>= \gss -> return $ (minimum $ map (\gs -> riskGS i gs) gss)

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
