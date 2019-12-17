module Main where

import System.Environment
import Lib

main :: IO ()
main = do
   args <- getArgs
   let
     chosenDay = head args
     (p1, p2) = daysMapping chosenDay
     dataFile = if length args > 1 then args!!1 else "./data/p" ++ chosenDay
   fullText <- readFile dataFile
   putStrLn $ p1 fullText
   putStrLn $ p2 fullText
