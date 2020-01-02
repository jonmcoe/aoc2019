module Days.Day09 where

import Data.Maybe (fromJust)

import Days.Common

day09a :: String -> String
day09a = show . fromJust . output . untilOutput . newComputerState [1] []

day09b :: String -> String
day09b = show . fromJust . output . untilHalt . newComputerState [2] []
