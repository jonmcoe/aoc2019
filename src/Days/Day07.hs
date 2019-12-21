module Days.Day07 where

import Data.List (maximumBy, permutations)
import Data.Ord (comparing)

import Days.Common

getOutputForPair :: (Int, Int) -> String -> Int
getOutputForPair (a,b) t = output $ compute $ newComputerState [a,b] t

chainCompute :: String -> [Int] -> Int
chainCompute t = foldl (\prevOutput ampSetting -> getOutputForPair (ampSetting, prevOutput) t) 0

day07a :: String -> String
day07a t = show $ maximumBy (comparing snd) $ map (\x -> (x, chainCompute t x)) $ permutations [0..4]


-- need to further generalize computer to yield FIRST output, not necessarily continue until halt command
-- i think we also need to maintain the same tape between runs
day07b :: String -> String
day07b t = show $ maximumBy (comparing snd) $ map (\x -> (x, chainCompute t x)) $ concatMap (generateRepetitions 1) (permutations [5..9])
  where generateRepetitions n l = map (\x -> concat $ replicate x l) [1..n]
