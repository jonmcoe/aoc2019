module Days.Day06 where

import Data.List.Split
import qualified Data.Map.Strict as M

type Orbit = (String, String)

root = "COM"

parse :: String -> [Orbit]
parse s = map (parseSingle . splitOn ")") $ lines s
  where parseSingle (a:b:_) = (a,b)

buildForwardMap :: [Orbit] -> M.Map String [String]
buildForwardMap = buildForwardMapImpl M.empty
  where
    buildForwardMapImpl acc ((orbitee, orbitor):rest) = buildForwardMapImpl updatedMap rest
      where
        updatedMap = M.alter elemAppend orbitee acc
        elemAppend Nothing = Just [orbitor]
        elemAppend (Just existing) = Just (orbitor:existing)
    buildForwardMapImpl acc [] = acc

sumDepth :: M.Map String [String] -> Int
sumDepth forwardMap = sumDepthImpl 0 root
  where
    sumDepthImpl curDepth curNode = curDepth + sum [sumDepthImpl (succ curDepth) nextUp | nextUp <- M.findWithDefault [] curNode forwardMap]

 -- parse, build map String -> [String], start at COM (guaranteed root) and fan out while incrementing depth and summing
day06a :: String -> String
day06a = show . sumDepth . buildForwardMap . parse

day06b :: String -> String
day06b = const "b"




---- relies on ordered input.. but we don't have that. we DO however have guaranteed COM as root
--buildDepthMap :: [Orbit] -> M.Map String Int
--buildDepthMap = buildDepthMapImpl M.empty
--  where
--    buildDepthMapImpl acc ((orbitee, orbitor):rest) = buildDepthMapImpl updatedMap rest
--      where updatedMap = M.insert orbitor (1 + M.findWithDefault 0 orbitee acc) acc
--    buildDepthMapImpl acc [] = acc
--
--day06a :: String -> String
--day06a = show . sum . M.elems . buildDepthMap . parse
