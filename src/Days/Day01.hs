module Days.Day01 where

readInt :: String -> Integer
readInt = read

gasCost :: Integer -> Integer
gasCost n = div n 3 - 2

gasCostCompounded :: Integer -> Integer
gasCostCompounded n
  | n < 9     = 0
  | otherwise = x + gasCostCompounded x where x = gasCost n -- TODO: what would be a recursion scheme?

allGasCost :: [Integer] -> Integer
allGasCost = sum . map gasCost

allGasCostCompounded :: [Integer] -> Integer
allGasCostCompounded = sum . map gasCostCompounded

parse = map readInt . lines

day01a :: String -> String
day01a = show . allGasCost . parse

day01b :: String -> String
day01b = show . allGasCostCompounded . parse
