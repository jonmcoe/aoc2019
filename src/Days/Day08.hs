module Days.Day08 where

import Data.Char
import Data.List (find, minimumBy, transpose)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Ord (comparing)

width = 25
height = 6

productOnesAndTwos l = numChar '1' l * numChar '2' l
numChar c = length . filter (== c)

day08a :: String -> String
day08a = show . productOnesAndTwos . minimumBy (comparing (numChar '0')) . chunksOf (width * height)

day08b :: String -> String
day08b = unlines . chunksOf width . map (substitute . fromJust . find (/= '2')) . transpose . chunksOf (width * height)
  where substitute x = " X" !! digitToInt x