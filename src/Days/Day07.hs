module Days.Day07 where

import Days.Common

day07a :: String -> String
day07a = show . output . compute . newComputerState [1,0]

day07b :: String -> String
day07b = show . output . compute . newComputerState [2,0]
