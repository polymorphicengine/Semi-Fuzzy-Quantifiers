module Main where

import Graphics.UI.Threepenny.Core hiding (Const)

import qualified Data.Map as Map

import Control.Monad

import Game
import GUI

-- main :: IO ()
-- main = do
--   let rule = [[D 0 0 0 0],[D 0 0 0 1, D 3 0 0 1]]
--       mode = BC_Rule rule
--       dom = Dom 1000
--       i = I [0..1000] [0..790]
--   v <- playBCMany 10000 rule dom i
--   putStrLn $ show v ++ "\n Exact: " ++ (show $ valBC_L (fromIntegral 2) (fromIntegral 1) (proportion i))

main :: IO ()
main = startGUI defaultConfig setup
