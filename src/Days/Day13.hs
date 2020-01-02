module Days.Day13 where

import Data.List (maximumBy, minimumBy, unfoldr, unlines)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

import Days.Common

type Tile = (Int, Int, Int)

nextTile :: ComputerState -> Maybe (Tile, ComputerState)
nextTile cs = case output firstState of
  Nothing          -> Nothing
  Just firstOutput -> Just ((firstOutput, fromJust $ output secondState, fromJust $ output thirdState), thirdState)
  where
    thirdState = untilOutput $ untilOutput $ untilOutput cs
    secondState = untilOutput $ untilOutput cs
    firstState = untilOutput cs

showPinball ph = unlines $ map makeRow [maxY,maxY-1..minY]
  where
    makeRow y = concatMap sqDisplay [(xx, y) | xx <- [minX..maxX]] ++ "\n"
    sqDisplay cell = " |=_O" !! M.findWithDefault 0 cell ph : "" -- extra space for readability
    maxY = snd $ maximumBy (comparing snd) $ M.keys ph
    minY = snd $ minimumBy (comparing snd) $ M.keys ph
    maxX = fst $ maximumBy (comparing fst) $ M.keys ph
    minX = fst $ minimumBy (comparing fst) $ M.keys ph

toMap l = M.fromList $ map newTup l
  where newTup (a,b,c) = ((a,b),c)

day13a = show . length . filter (\x -> getThird x == 2) . unfoldr nextTile . newComputerState [] []
  where getThird (_, _, x) = x
day13b = showPinball . toMap . unfoldr nextTile . newComputerState (repeat 0) [(0,2)] -- TODO: start "playing" the game