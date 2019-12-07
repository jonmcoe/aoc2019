module Days.Day03 where

import Data.List.Split
import qualified Data.Set as S

readInt :: String -> Integer
readInt = read

data Direction = Righty | Lefty | Up | Down deriving Show

charToDirection :: Char -> Direction
charToDirection 'R' = Righty
charToDirection 'L' = Lefty
charToDirection 'U' = Up
charToDirection 'D' = Down
charToDirection _ = error "can't parse"

--pathSet :: [(Direction, Int)] -> (Int, Int) -> S.Set[(Int, Int)] -> S.Set[(Int, Int)]
--pathSet [] _ _ = S.empty
--pathSet (d:ds) (x, y) soFar = S.union soFar () -- pattern match for drawing them all out by direction...

parse = map (map r . splitOn ",") . lines
  where
    r s = (charToDirection (head s), readInt (tail s))

day03a = show . parse
day03b = show . parse