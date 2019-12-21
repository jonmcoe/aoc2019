module Days.Day07 where

import Data.List (maximumBy, permutations)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Days.Common

getOutputForPair :: (Int, Int) -> String -> Maybe Int
getOutputForPair (a,b) = output . nextOutput . newComputerState [a,b]

chainCompute :: String -> [Int] -> Int
chainCompute t = foldl (\prevOutput ampSetting -> fromJust $ getOutputForPair (ampSetting, prevOutput) t) 0

runFeedbackLoop :: String -> [Int] -> Int
runFeedbackLoop t params = fromJust $ output $ (M.! 4) $ runFeedbackLoopImpl initialCsList 0
  where
    runFeedbackLoopImpl csList step = case output curResult of
      Nothing  -> csList
      Just csO -> runFeedbackLoopImpl
                   (M.adjust
                    (appendToInput csO)  -- append input to...
                    (succ step `mod` 5)  -- ... the next step's machine
                    (M.insert (step `mod` 5) curResult csList) -- update our just-run machine
                   )
                   (succ step)
      where
        curResult = nextOutput $ csList M.! (step `mod` 5)
    initialCsList = M.fromList [ (0, newComputerState [head params, 0] t)
                               , (1, newComputerState [params!!1] t)
                               , (2, newComputerState [params!!2] t)
                               , (3, newComputerState [params!!3] t)
                               , (4, newComputerState [params!!4] t)
                               ]

day07a :: String -> String
day07a = show . maximum . flip map (permutations [0..4]) . chainCompute


day07b :: String -> String
day07b = show . maximum . flip map (permutations [5..9]) . runFeedbackLoop


-- -- --
--permRepetitons = concatMap (generateRepetitions 1) (permutations [5..9])
--generateRepetitions n l = map (\x -> concat $ replicate x l) [1..n]
