module Days.Day02 where

import Data.List (find)

import Days.Common

subAndCompute :: [Int] -> (Int, Int) -> Int
subAndCompute l (noun, verb) = head $ tape endState
  where
    endState         = compute substitutedState
    substitutedState = newComputerStateParsedTape $ concat [take 1 l, [noun, verb], drop 3 l]

search :: [Int] -> Int -> Int -> Maybe (Int, Int)
search l target limit = find (\(x1,y1) -> subAndCompute l (x1, y1) == target) [(x,y) | x <- [1..limit], y <- [1..limit]]

nounVerbSum :: Maybe (Int,Int) -> Int
nounVerbSum m = case m of
    Just (a,b) -> a * 100 + b
    Nothing    -> -1

day02a :: String -> String
day02a t = show $ subAndCompute (parseComputerTape t) (12, 2)

day02b :: String -> String
day02b t = show $ nounVerbSum $ search (parseComputerTape t) 19690720 100
