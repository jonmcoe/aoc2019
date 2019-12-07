module Days.Day02 where

import Data.List.Split
import Data.List (find)

replaceAtIndex l i v = concat [take i l, [v], drop (i + 1) l]

compute :: Int -> [Int] -> [Int]
compute i l
  | l!!i == 1  = compute (i + 4) (replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) + l!!(l!!(i + 2))))
  | l!!i == 2  = compute (i + 4) (replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) * l!!(l!!(i + 2))))
  | l!!i == 99 = l
  | otherwise = error "wah"

subAndCompute :: [Int] -> (Int, Int) -> [Int]
subAndCompute l (noun, verb) = compute 0 $ concat [take 1 l, [noun, verb], drop 3 l]

search :: [Int] -> Int -> Int -> Maybe (Int, Int)
search l target limit = find (\(x1,y1) -> head (subAndCompute l (x1, y1)) == target) [(x,y) | x <- [1..limit], y <- [1..limit]]

nounVerbSum :: Maybe (Int,Int) -> Int
nounVerbSum m = case m of
    Just (a,b) -> a * 100 + b
    Nothing    -> -1

parsed t = map r $ splitOn "," t where r x = read x::Int

day02a :: String -> String
day02a t = show $ head $ subAndCompute (parsed t) (12, 2)

day02b :: String -> String
day02b t = show $ nounVerbSum $ search (parsed t) 19690720 100

