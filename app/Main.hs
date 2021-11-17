module Main where

import Graphics.UI.Threepenny.Core hiding (Const)

import Game
import GUI

-- main :: IO ()
-- main = do
--   let f = Quant WOR 5 5 (Var "x") (Pred "TOP" []) (Pred "A" [V (Var "x")])
--       i = Map.insert "B" [[2],[1],[0],[3]] $ Map.singleton "A" [[0],[1],[2],[3],[4],[5],[6]] :: Interpretation
--   v <- fmap sum $ replicateM 100000 (play f (Dom 8) i)
--   let x = v/100000
--   putStrLn $ show $ 1 - x
--   -- putStrLn $ show $ val 2 1 (2/4)
--   putStrLn $ show $ valWOR 8 5 5 (7/8)

main :: IO ()
main = startGUI defaultConfig setup
