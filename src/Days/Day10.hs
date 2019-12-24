{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Days.Day10 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

data Orientation = Leftwards | Rightwards deriving (Eq, Ord, Show)

type Point a = (a, a)
type Slope a = (Orientation, Maybe a) -- Record might be neater, but we get Ord for free with tuple in this order

parse :: (Fractional a, Enum a) => String -> [Point a]
parse = concat . zipWith (\a b -> map (, a) b) [0..] . map processRowElements . lines
  where
    processRowElements row = map fst . filter ((== '#') . snd) $ zip [0..] row

slope :: (Eq a, Fractional a, Ord a) => Point a -> Point a -> Slope a
slope (x1, y1) (x2, y2) = (orientation, magnitude)
  where
    magnitude = if x1 == x2 then Nothing else Just ((y2 - y1) / (x2 - x1))
    orientation
      | x1 > x2 = Leftwards
      | x1 < x2 = Rightwards
      | y1 > y2 = Leftwards
      | y1 < y2 = Rightwards

pairsWith :: (Eq a, Fractional a) => Int -> [Point a] -> [(Point a, Point  a)]
pairsWith i l = [(l!!i, l!!j) | j <- [0..length l - 1], j /= i]

otherSlopes i l = map (uncurry slope) (pairsWith i l)

numUniqueOtherSlopes :: (Eq a, Fractional a, Ord a) => Int -> [Point a] -> Int
numUniqueOtherSlopes i l = length $ S.fromList $ otherSlopes i l

day10a s = show $ maximum $ map (`numUniqueOtherSlopes` points) [0..length points - 1]
  where points = parse s

day10b = show . parse
--  where
--    startingPoint =
--    points        = parse s