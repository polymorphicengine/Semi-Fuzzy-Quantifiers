module Main where

import Graphics.UI.Threepenny.Core hiding (Const)

import qualified Data.Map as Map

import Control.Monad

import Game
import GUI

-- main :: IO ()
-- main = do
--   let f = Quant WR 1 1 (Var "x") (Pred "B" []) (Pred "A" [V (Var "x")])
--       i = Map.insert "B" [] $ Map.singleton "A" [[0],[1],[2],[3],[4]] :: Interpretation
--   v <- fmap sum $ replicateM 100000 (play f (Dom 10) i)
--   let x = v/100000
--   putStrLn $ show $ 1 - x
--   -- putStrLn $ show $ val 2 1 (2/4)
--   putStrLn $ show $ valWOR 8 5 5 (7/8)

main :: IO ()
main = startGUI defaultConfig setup
