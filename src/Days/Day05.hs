module Days.Day05 where

import Days.Common
import Data.Maybe (fromJust)

day05a :: String -> String
day05a = show . fromJust . output . compute . newComputerState [1]

day05b :: String -> String
day05b = show . fromJust . output . compute . newComputerState [5]
