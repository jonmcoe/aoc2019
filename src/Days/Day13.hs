module Days.Day13 where

import Data.List (unfoldr)
import Data.Maybe

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

allTiles = unfoldr nextTile

day13a = show . length . filter (\x -> getThird x == 2) . allTiles . newComputerState []
  where getThird (_, _, x) = x
day13b = const ""