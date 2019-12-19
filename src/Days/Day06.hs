module Days.Day06 where

import Data.List.Split
import qualified Data.Map.Strict as M

type Orbit = (String, String)

parse :: String -> [Orbit]
parse s = map (parseSingle . splitOn ")") $ lines s
  where parseSingle (a:b:_) = (a,b)

day06a :: String -> String
day06a = const "a"

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
