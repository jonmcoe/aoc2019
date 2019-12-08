module Days.Day03 where

import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type PathEntry = (Int, Int)

data Direction = Righty | Lefty | Up | Down deriving Show

pathEntryToDistance :: [(Direction, Int)] -> M.Map PathEntry Int
pathEntryToDistance = fst . foldl f (M.empty, ((0,0),0))
  where
    f (soFar, ((oldMostRecentX, oldMostRecentY), oldMostRecentMag)) incoming = (M.union soFar (M.fromList additions), newMostRecent)
      where
        newMostRecent = last additions
        additions = case incoming of
          (Righty, mag) -> [((oldMostRecentX + i, oldMostRecentY), oldMostRecentMag + i) | i <- [1..mag]]
          (Lefty, mag)  -> [((oldMostRecentX - i, oldMostRecentY), oldMostRecentMag + i) | i <- [1..mag]]
          (Up, mag)     -> [((oldMostRecentX, oldMostRecentY + i), oldMostRecentMag + i) | i <- [1..mag]]
          (Down, mag)   -> [((oldMostRecentX, oldMostRecentY - i), oldMostRecentMag + i) | i <- [1..mag]]

intersections :: String -> M.Map PathEntry Int
intersections = foldl1 (M.intersectionWith (+)) . map pathEntryToDistance . parse
  where
    parse = map (map r . splitOn ",") . lines
    r s = (charToDirection (head s), read (tail s)::Int)
    charToDirection 'R' = Righty
    charToDirection 'L' = Lefty
    charToDirection 'U' = Up
    charToDirection 'D' = Down
    charToDirection _ = error "can't parse"

manhattanDistance :: PathEntry -> Int
manhattanDistance (x, y) = abs x + abs y

day03a = show . minimum . map manhattanDistance . M.keys . intersections
day03b = show . minimum . M.elems . intersections
