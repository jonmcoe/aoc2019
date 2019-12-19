module Lib where

import Days.Day01 (day01a, day01b)
import Days.Day02 (day02a, day02b)
import Days.Day03 (day03a, day03b)
import Days.Day04 (day04a, day04b)
import Days.Day05 (day05a, day05b)
import Days.Day06 (day06a, day06b)

-- TODO: if we can allow any showable to return, then no need for show in all the day solutions
daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping "02" = (day02a, day02b)
daysMapping "03" = (day03a, day03b)
daysMapping "04" = (day04a, day04b)
daysMapping "05" = (day05a, day05b)
daysMapping "06" = (day06a, day06b)
daysMapping _    = error "not implemented"
