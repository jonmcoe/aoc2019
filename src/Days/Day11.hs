module Days.Day11 where

import Data.List (minimumBy, maximumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)


import Days.Common

type Color = Int
type Point = (Int, Int)
type PaintedHull = M.Map Point Color

paintedHull :: PaintedHull -> ComputerState -> PaintedHull
paintedHull acc = paintedHullImpl acc (0,1) (0,0)

paintedHullImpl :: PaintedHull -> Point -> Point -> ComputerState -> PaintedHull
paintedHullImpl acc bearing loc cs = case output nextCs of
  Just o   -> paintedHullImpl (M.insert loc o acc) nextBearing nextLoc nextNextCs
  Nothing -> acc
  where
    nextLoc = (fst loc + fst nextBearing, snd loc + snd nextBearing)
    nextBearing = case output nextNextCs of -- 0: left, 1: right
      Just 0 -> (snd bearing * (-1), fst bearing)
      Just 1 -> (snd bearing, fst bearing * (-1))
    nextNextCs = untilOutput nextCs
    nextCs = untilOutput (appendToInput curColor cs)
    curColor = M.findWithDefault 0 loc acc

showPaintedHull ph = unlines $ map makeRow [maxY,maxY-1..minY]
  where
    makeRow y = map sqDisplay [(xx, y) | xx <- [minX..maxX]] ++ "\n"
    sqDisplay cell = if M.findWithDefault 0 cell ph == 1 then '#' else ' '
    maxY = snd $ maximumBy (comparing snd) $ M.keys ph
    minY = snd $ minimumBy (comparing snd) $ M.keys ph
    maxX = fst $ maximumBy (comparing fst) $ M.keys ph
    minX = fst $ minimumBy (comparing fst) $ M.keys ph

day11a = show . length . M.keys . paintedHull M.empty . newComputerState []
day11b = showPaintedHull . paintedHull (M.singleton (0,0) 1) . newComputerState []
