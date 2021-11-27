module Main where

import Graphics.UI.Threepenny.Core hiding (Const)

import qualified Data.Map as Map

import Control.Monad

import Game
import GUI

-- main :: IO ()
-- main = do
--   xs <- drawWR (Dom 10) (I [0..9] []) 10
--   putStrLn $ show xs

main :: IO ()
main = startGUI defaultConfig setup
