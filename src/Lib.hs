module Lib where

import Days.Day01 (day01a, day01b)
import Days.Day02 (day02a, day02b)
import Days.Day03 (day03a, day03b)
import Days.Day04 (day04a, day04b)
import Days.Day05 (day05a, day05b)
import Days.Day06 (day06a, day06b)
import Days.Day07 (day07a, day07b)
import Days.Day08 (day08a, day08b)
import Days.Day09 (day09a, day09b)
import Days.Day10 (day10a, day10b)
import Days.Day11 (day11a, day11b)
import Days.Day12 (day12a, day12b)
import Days.Day13 (day13a, day13b)

-- TODO: if we can allow any showable to return, then no need for show in all the day solutions
daysMapping :: String -> (String -> String, String -> String)
daysMapping "01" = (day01a, day01b)
daysMapping "02" = (day02a, day02b)
daysMapping "03" = (day03a, day03b)
daysMapping "04" = (day04a, day04b)
daysMapping "05" = (day05a, day05b)
daysMapping "06" = (day06a, day06b)
daysMapping "07" = (day07a, day07b)
daysMapping "08" = (day08a, day08b)
daysMapping "09" = (day09a, day09b)
daysMapping "10" = (day10a, day10b)
daysMapping "11" = (day11a, day11b)
daysMapping "12" = (day12a, day12b)
daysMapping "13" = (day13a, day13b)
daysMapping _    = error "not implemented"
