module Main where

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

main :: IO ()
main = do
  text <- readFile "/Users/jon/repos/aoc2019/data/p01"
  let
    x = map readInt $ lines text
    p1 = allGasCost x
    p2 = allGasCostCompounded x
  print [p1, p2]
