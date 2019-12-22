module Days.Day09 where

import Days.Common

day09a :: String -> String
day09a = show . output . untilOutput . newComputerState [1]

day09b :: String -> String
day09b = show . output . untilOutput . newComputerState [1]
