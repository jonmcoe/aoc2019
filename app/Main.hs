module Main where

import System.Environment
import Data.List.Split
import Data.List (find)
import qualified Data.Set as S

-- p1

readInt :: String -> Integer
readInt = read

gasCost :: Integer -> Integer
gasCost n = div n 3 - 2

gasCostCompounded :: Integer -> Integer
gasCostCompounded n
  | n < 9     = 0
  | otherwise = x + gasCostCompounded x where x = gasCost n -- what would be a recursion scheme?

allGasCost :: [Integer] -> Integer
allGasCost = sum . map gasCost

allGasCostCompounded :: [Integer] -> Integer
allGasCostCompounded = sum . map gasCostCompounded

m1 = do
  args <- getArgs
  text <- readFile (head args :: FilePath)
  let
    x = map readInt $ lines text
    p1 = allGasCost x
    p2 = allGasCostCompounded x
  print [p1, p2]

-- p2
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

m2 = do
  args <- getArgs
  text <- readFile (head args :: FilePath)
  let
    l = map r $ splitOn "," text where r x = read x::Int
    p1 = head $ subAndCompute l (12, 2)
    p2 = nounVerbSum $ search l 19690720 100
  print [p1, p2]


-- p3

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

m3 = do
  args <- getArgs
  text <- readFile (head args :: FilePath)
  let
    llines = lines text
    l = map (map r . splitOn ",") llines where r s = (charToDirection (head s), readInt (tail s))
--    fullPaths = map () -- draw out all paths
--    p1 =
  print l
--  print [p1, p2]


main :: IO ()
main = m3
