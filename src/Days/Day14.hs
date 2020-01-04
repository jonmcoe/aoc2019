module Days.Day14 where

import qualified Data.Map.Strict as M

type Requirement = (String, Integer)
type RequirementMap = M.Map String Integer
type ResourcesMap = M.Map String (Integer, RequirementMap)

-- TODO: parse
dataCheating :: ResourcesMap
dataCheating = M.fromList [ ("A",    (2, M.fromList [("ORE", 9)])) -- 1:9, 2:9, 3:18, 4:18
                          , ("B",    (3, M.fromList [("ORE", 8)]))
                          , ("C",    (5, M.fromList [("ORE", 7)]))
                          , ("AB",   (1, M.fromList [("A", 3), ("B", 4)]))
                          , ("BC",   (1, M.fromList [("B", 5), ("C", 7)]))
                          , ("CA",   (1, M.fromList [("C", 4), ("A", 1)]))
                          , ("FUEL", (1, M.fromList [("AB", 2), ("BC", 3), ("CA", 4)]))
                          ]

-- TODO: acc and excess resource sharing... damn
endRequirements :: ResourcesMap -> String -> Integer -> RequirementMap
endRequirements rMap resource amt
  | M.keys currentMap == ["ORE"] = M.singleton "ORE" (amtNeeded amt currentYield "ORE")
  | otherwise                    = M.unionsWith (+) [endRequirements rMap r (amtNeeded amt currentYield r) | r <- M.keys currentMap]
  | otherwise = M.empty
  where
    amtNeeded target yield el = ceiling (fromInteger target / fromInteger yield) * (currentMap M.! el)
    (currentYield, currentMap) = rMap M.! resource

day14a _ = show $ endRequirements dataCheating "FUEL" 1
day14b = id
