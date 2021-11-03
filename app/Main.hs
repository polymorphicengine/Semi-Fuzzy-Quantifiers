module Main where

import qualified Data.Map as Map
import Data.List
import System.Random
import Control.Monad

main :: IO ()
main = do
  let ini = (initialGameBoard $ Quant 2 1 (Var "x") (P (Pred "A" [V (Var "x")])))
  gt <- expandGameTree (Dom 5) ini
  putStrLn $ show gt

type Id = String

newtype Var = Var Id deriving (Eq,Ord)

newtype Const = Const Int deriving (Eq)

data Term = V Var | C Const deriving (Eq)

data Pred = Pred Id [Term] deriving (Eq)

data Formula = P Pred | Quant {iPick :: Int,
                               uPick :: Int,
                               bVar :: Var,
                               formula :: Formula
                              }
                              deriving Eq

-- the domain for an integer n is actually [0..n-1]
newtype Domain = Dom Int deriving (Eq, Show)

type Interpretation = Map.Map Id [[Int]]

type Assignment = Map.Map Var Int

data GameState = GS [Formula] [Formula] deriving (Eq, Show)

data GameTree = Leaf GameState | Branch GameState [GameTree] deriving (Eq, Show)


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
  show (Quant k m x f) = "Q" ++ "^" ++ show k ++ "_" ++ show m ++ " " ++ show x ++ "." ++ show f



-- P (Pred "A" [C (Const 3)])

-- Quant 2 1 "x" (P (Pred "A" [V (Var "x")]))

containsFV :: Assignment -> [Term] -> Bool
containsFV _ [] = False
containsFV a ((V x):xs) = case Map.lookup x a of
                                            Just _ -> False
                                            Nothing -> True
containsFV a ((C _):xs) = containsFV a xs

assign :: Assignment -> [Term] -> [Int]
assign a [] = []
assign a ((C (Const i)):xs) = i:(assign a xs)
assign a ((V x):xs) = case Map.lookup x a of
                                    Just i -> i:(assign a xs)
                                    Nothing -> error "Not assigned variable"


interpretAtom :: Interpretation -> Assignment -> Pred -> Double
interpretAtom interp a (Pred s terms) | containsFV a terms = error "Not closed predicate"
                                      | otherwise = do
                                              case Map.lookup s interp of
                                                  Just lss -> if elem (assign a terms) lss then 1 else 0
                                                  Nothing -> error "Undefined predicate symbol"



substituteT :: Id -> Int -> [Term] -> [Term]
substituteT _ _ [] = []
substituteT s i (c@(C _):xs) = c:(substituteT s i xs)
substituteT s i (v@(V (Var x)):ts) = if s == x then (C (Const i)):(substituteT s i ts) else v:(substituteT s i ts)

substitute :: Id -> Int -> Formula -> Formula
substitute s i (P (Pred p terms)) = (P (Pred p (substituteT s i terms)))
substitute s i (Quant k m v@(Var x) f) | s /= x = Quant k m v (substitute s i f)
                                       | otherwise = (substitute s i f)



drawOnce :: Domain -> IO Int
drawOnce (Dom n) = getStdRandom (randomR (0,n-1))

-- n will be k+m
draw :: Domain -> Int -> IO [Int]
draw d n = replicateM n $ drawOnce d

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

stripQuantifier :: Formula -> (Id, Formula)
stripQuantifier (P _) = error "No quantifier to strip"
stripQuantifier (Quant _ _ (Var x) f) = (x,f)


initialGameBoard :: Formula -> GameTree
initialGameBoard f = Leaf (GS [] [f])


isAtom :: Formula -> Bool
isAtom (P _) = True
isAtom _ = False

expandFormula :: Domain -> Formula -> IO [GameState]
expandFormula _ (P _) = error "Can't be called on atoms"
expandFormula d (Quant k m (Var x) f) = do
                                    is <- draw d (k+m)
                                    let subs = genSubsetsKM k is
                                        gameStates = map (\sub -> plugInOnce x sub f) subs
                                    return gameStates

selectFirst :: [Formula] -> Formula
selectFirst [] = error "Something went wrong"
selectFirst (f:fs) | isAtom f = selectFirst fs
                   | otherwise = f

expandGameState :: Domain -> GameState -> IO (Maybe [GameState])
expandGameState d (GS us is) | all isAtom (us++is) = return Nothing
                             | otherwise = do
                                  let f = selectFirst (us++is)
                                  fmap Just $ expandFormula d f

expandGameTree :: Domain -> GameTree -> IO GameTree
expandGameTree d (Leaf g) = do
                  mayGS <- expandGameState d g
                  case mayGS of
                    Nothing -> return (Leaf g)
                    (Just gs) -> do
                          gts <- sequence (map (expandGameTree d) (map Leaf gs))
                          return $ Branch g gts
expandGameTree _ gt = return gt
