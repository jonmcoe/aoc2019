module Days.Day07 where

import Data.List (maximumBy, permutations)
import Data.Maybe (fromJust)
import Data.Sequence as S

import Days.Common

chainCompute :: String -> [Int] -> Int
chainCompute t = foldl (\prevOutput ampSetting -> getOutputForPair (ampSetting, prevOutput) t) 0
  where getOutputForPair (a,b) = fromJust . output . untilOutput . newComputerState [a,b]

runFeedbackLoop :: String -> [Int] -> Int
runFeedbackLoop t params = fromJust $ output $ S.index (runFeedbackLoopImpl initialCsList 0) 4
  where
    runFeedbackLoopImpl csList step = case output curResult of
      Nothing  -> csList
      Just csO -> runFeedbackLoopImpl
                    (S.adjust' (appendToInput csO)         -- append input to
                      nextIndex                            -- the next step's machine
                      (S.update curIndex curResult csList) -- after updating our just-run machine
                    )
                    (succ step)
      where
        curResult = untilOutput $ S.index csList curIndex
        curIndex  = step `mod` 5
        nextIndex = succ step `mod` 5
    initialCsList = S.fromList [ newComputerState [head params, 0] t
                               , newComputerState [params!!1] t
                               , newComputerState [params!!2] t
                               , newComputerState [params!!3] t
                               , newComputerState [params!!4] t
                               ]

day07a :: String -> String
day07a = show . maximum . flip map (permutations [0..4]) . chainCompute


day07b :: String -> String
day07b = show . maximum . flip map (permutations [5..9]) . runFeedbackLoop


-- -- --
--permRepetitons = concatMap (generateRepetitions 1) (permutations [5..9])
--generateRepetitions n l = map (\x -> concat $ replicate x l) [1..n]
