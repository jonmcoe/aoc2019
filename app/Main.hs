module Main where

import System.Environment
import Data.List.Split

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

--searchSum :: [Int] -> Int -> Int
--searchAndSum l target =

m2 = do
  args <- getArgs
  text <- readFile (head args :: FilePath)
  let
    l = map r $ splitOn "," text where r x = read x::Int
    p1 = head $ subAndCompute l (12, 2)
    p2 = 3
  print [p1, p2]


main :: IO ()
main = m2
