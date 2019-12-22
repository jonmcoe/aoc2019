module Days.Day02 where

import Data.List (find)
import Data.Maybe (fromJust)

import Days.Common


search :: Int -> Int -> String -> Maybe (Int, Int)
search target limit l = find (\(x1,y1) -> subAndCompute (x1, y1) l == target) [(x,y) | x <- [1..limit], y <- [1..limit]]

nounVerbSum :: Maybe (Int,Int) -> Maybe Int
nounVerbSum m = case m of
    Just (noun, verb) -> Just (noun * 100 + verb)
    Nothing           -> Nothing

day02a :: String -> String
day02a = show . subAndCompute (12, 2)

day02b :: String -> String
day02b = show . fromJust . nounVerbSum . search 19690720 100
