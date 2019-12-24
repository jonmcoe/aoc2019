{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Days.Day10 where

import Data.List (elemIndex, maximumBy, nub)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Sort (sort)
import qualified Data.Set as S

data Orientation = Rightwards | Leftwards deriving (Eq, Ord, Show)

type Point a = (a, a)
data Slope a = Slope {orientation :: Orientation, direction :: Maybe a} deriving (Eq, Ord, Show)

parse :: (Fractional a, Enum a) => String -> [Point a]
parse = concat . zipWith (\a b -> map (, a) b) [0..] . map processRowElements . lines
  where
    processRowElements row = map fst . filter ((== '#') . snd) $ zip [0..] row

slope :: (Eq a, Fractional a, Ord a) => Point a -> Point a -> Slope a
slope (x1, y1) (x2, y2) = Slope {orientation, direction}
  where
    direction = if x1 == x2 then Nothing else Just ((y2 - y1) / (x2 - x1)) -- this is messy. y is "downwards" so this SHOULD be reversed, but also we want positives to come before negatives in the ordering, so we are sort of wrong twice here and it works out
    orientation
      | x1 > x2 = Leftwards
      | x1 < x2 = Rightwards
      | y1 > y2 = Leftwards
      | y1 < y2 = Rightwards

pairsWith :: (Eq a, Fractional a) => Int -> [Point a] -> [(Point a, Point a)]
pairsWith i l = [(l!!i, l!!j) | j <- [0..length l - 1], j /= i]

otherSlopes i l = map (uncurry slope) (pairsWith i l)

numUniqueOtherSlopes :: (Eq a, Fractional a, Ord a) => Int -> [Point a] -> Int
numUniqueOtherSlopes i l = length $ nub $ otherSlopes i l

day10a s = show $ snd $ maximumBy (comparing snd) $ map (\i -> (points!!i, numUniqueOtherSlopes i points)) [0..length points - 1]
  where points = parse s

-- one "lap" is sufficient
day10b s = show candidatesHavingSlope
  where
    candidatesHavingSlope = filter (\candidate -> candidate /= startingPoint && slope startingPoint candidate == desiredSlope ) points
    desiredSlope = sort (nub (otherSlopes startingPointIndex points)) !! 199
    startingPointIndex = fromJust (elemIndex startingPoint points)
    startingPoint = fst $ maximumBy (comparing snd) $ map (\i -> (points!!i, numUniqueOtherSlopes i points)) [0..length points - 1]
    points        = parse s