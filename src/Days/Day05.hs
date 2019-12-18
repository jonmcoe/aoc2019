module Days.Day05 where

import Days.Common

day05a :: String -> String
day05a t = show $ output $ compute ComputerState {position = 0, tape = parseComputerTape t, input = 1, output = -1}

day05b :: String -> String
day05b t = "x"
