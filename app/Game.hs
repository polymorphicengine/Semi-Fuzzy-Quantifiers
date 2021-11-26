module Game where

import qualified Data.Map as Map
import Data.List
import System.Random
import Control.Monad

import Data.Maybe


type Id = String

newtype Var = Var Id deriving (Eq,Ord)

newtype Const = Const Int deriving (Eq)

data Term = V Var | C Const deriving (Eq)

data Pred = Pred Id [Term] deriving (Eq)

data Mode = WR | WOR | BC_L | BC_G deriving Eq

data Formula = Bot | P Pred | Quant {mode :: Mode,
                               iPick :: Int,
                               uPick :: Int,
                               bVar :: Var,
                               range :: Pred,
                               scope :: Pred
                              }
                              deriving Eq

-- the domain for an integer n is actually [0..n-1]
newtype Domain = Dom Int deriving (Eq, Show)

type Interpretation = Map.Map Id [[Int]]

type Assignment = Map.Map Var Int

data GameState = GS [Formula] [Formula] | Stop deriving (Eq, Show)

-- prettier show instances

instance Show Var where
  show (Var x) = x

instance Show Const where
  show (Const i) = show i

instance Show Term where
  show (C c) = show c
  show (V v) = show v

instance Show Pred where
  show (Pred a ts) = a ++ "(" ++ intercalate "," (map show ts) ++ ")"

instance Show Formula where
  show (P p) = show p
  show (Quant _ k m x r s) = "Q" ++ "^" ++ show k ++ "_" ++ show m ++ " " ++ show x ++ "." ++ show r ++ "." ++ show s
  show Bot = "Bot"

containsFV :: [Term] -> Bool
containsFV [] = False
containsFV ((V x):xs) = True
containsFV ((C _):xs) = containsFV xs


unsafeTerms :: [Term] -> [Int]
unsafeTerms [] = []
unsafeTerms ((C (Const i)):xs) = i:(unsafeTerms xs)
unsafeTerms _ = error "Free variable encountered"


atomRisk :: Interpretation -> Formula -> Double
atomRisk interp (P (Pred s terms)) | containsFV terms = error "Not closed predicate"
                                   | otherwise = do
                                          case Map.lookup s interp of
                                              Just lss -> if elem (unsafeTerms terms) lss then 0 else 1
                                              Nothing -> error "Undefined predicate symbol"
atomRisk _ Bot = 1
atomRisk _ _ = error "Cannot evaluate a gamestate that is not fully expanded"


substituteT :: Id -> Int -> [Term] -> [Term]
substituteT _ _ [] = []
substituteT s i (c@(C _):xs) = c:(substituteT s i xs)
substituteT s i (v@(V (Var x)):ts) = if s == x then (C (Const i)):(substituteT s i ts) else v:(substituteT s i ts)

substitute :: Id -> Int -> Formula -> Formula
substitute s i (P (Pred p terms)) = (P (Pred p (substituteT s i terms)))
substitute s i f = f


drawOnce :: Domain -> Interpretation -> Pred -> IO Int
drawOnce d@(Dom n) i p@(Pred a _) = do
              x <- getStdRandom (randomR (0,n-1))
              case a == "TOP" of
                True -> return x
                False -> case Map.lookup a i of
                    Just [] -> error "The range predicate should not be falsum"
                    Just iss -> case elem [x] iss of
                          True -> return x
                          False -> drawOnce d i p
                    Nothing -> error "Undefined range predicate"

-- n will be k+m
drawWR :: Domain -> Interpretation -> Pred -> Int -> IO [Int]
drawWR d i p n = replicateM n $ drawOnce d i p

drawWOR :: Domain -> Interpretation -> Pred -> Int -> [Int] -> IO [Int]
drawWOR d i p 0 is = return []
drawWOR d@(Dom k) i p n is = case n > k of
                              True -> error "Can't choose that many constants without repetition"
                              False -> do
                                    x <- drawOnce d i p
                                    case elem x is of
                                      True -> drawWOR d i p n is
                                      False -> do
                                            xs <- drawWOR d i p (n-1) (x:is)
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

plugInOnce :: Id -> ([Int],[Int]) -> Formula -> GameState
plugInOnce x (us,is) f = GS (map (\c -> substitute x c f) us) (map (\c -> substitute x c f) is)

stripQuantifier :: Formula -> (Id, Pred, Pred)
stripQuantifier (P _) = error "No quantifier to strip"
stripQuantifier (Quant _ _ _ (Var x) rP sP) = (x,rP,sP)


isAtom :: Formula -> Bool
isAtom (P _) = True
isAtom Bot = True
isAtom _ = False

expandFormula :: Domain -> Interpretation -> Formula -> IO [GameState]
expandFormula _ _ (P _) = error "Can't be called on atoms"
expandFormula d i (Quant mode k m (Var x) rP sP) = case mode of
                                        WR -> do
                                            is <- drawWR d i rP (k+m)
                                            let subs = genSubsetsKM k is
                                                gameStates = map (\(GS xs ys) -> GS xs (ys ++ (replicate m Bot))) $ map (\sub -> plugInOnce x sub (P sP)) subs
                                            return $ gameStates ++ [Stop]
                                        WOR -> do
                                            is <- drawWOR d i rP m []
                                            let subs = [([],is) | (_,is) <- genSubsetsKM k is]
                                                gameStates = map (\sub -> plugInOnce x sub (P sP)) subs
                                            return $ gameStates ++ [Stop]
                                        BC_L -> do
                                          us <- drawWR d i rP (k+m)
                                          let yourFormulas = (map (\c -> substitute x c (P sP)) us)
                                              gameState = GS yourFormulas (replicate m Bot)
                                              val = - (fromIntegral k) + (fromIntegral $ m+k)*(proportion i)
                                          return $ if val <= 0 then [GS [] []] else if val <= 1 then [gameState] else [Stop]
                                        BC_G -> do
                                          us <- drawWR d i rP (k+m)
                                          let myFormulas = (map (\c -> substitute x c (P sP)) us)
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
proportion i = case Map.lookup "R" i of
                    Nothing -> error "oops"
                    Just rs -> case Map.lookup "S" i of
                      Nothing -> error "oops"
                      Just ss -> (fromIntegral $ length int) / (fromIntegral $ length rs)
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
