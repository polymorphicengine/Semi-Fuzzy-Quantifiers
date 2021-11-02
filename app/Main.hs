module Main where

import Data.Map as Map

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Id = String

newtype Var = Var Id deriving (Eq,Show,Ord)

newtype Const = Const Int deriving (Eq,Show)

data Term = V Var | C Const deriving (Eq,Show)

data Pred = Pred Id [Term] deriving (Eq, Show)

data Formula = P Pred | Quant {iPick :: Int,
                               uPick :: Int,
                               bVar :: Var,
                               formula :: Formula
                              }
                              deriving (Eq, Show)

-- the domain for an integer n is actually [0..n-1]
newtype Domain = Dom Int deriving (Eq, Show)

type Interpretation = Map Id [[Int]]

type Assignment = Map Var Int

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


interpretAtom :: Interpretation -> Assignment -> Pred -> IO Double
interpretAtom interp a (Pred s terms) | containsFV a terms = error "Not closed predicate"
                                      | otherwise = do
                                              case Map.lookup s interp of
                                                  Just lss -> return $ if elem (assign a terms) lss then 1 else 0
                                                  Nothing -> error "Undefined predicate symbol"
  -- interpret' (Dom n) a (Quant k m x f)

-- isClosed :: Formula -> Bool
