module Days.Day05 where

import Days.Common

day05a :: String -> String
day05a = show . output . compute . newComputerState 1

day05b :: String -> String
day05b = show . output . compute . newComputerState 5
