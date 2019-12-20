module Days.Day06 where

import Data.List (find, elem, elemIndex, unfoldr)
import Data.List.Split
import Data.Maybe (fromJust)
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

buildReverseMap :: [Orbit] -> M.Map String String
buildReverseMap = buildReverseMapImpl M.empty
  where
    buildReverseMapImpl acc ((orbitee, orbitor):rest) = buildReverseMapImpl (M.insert orbitor orbitee acc) rest
    buildReverseMapImpl acc [] = acc

 -- TODO: fix. it misses final (root) element. probably won't matter to solution, but why?
ancestorChain :: M.Map String String -> String -> [String]
ancestorChain m = unfoldr (\s -> if M.member s m then Just (s, m M.! s) else Nothing)

sumDepth :: M.Map String [String] -> Int
sumDepth forwardMap = sumDepthImpl 0 root
  where
    sumDepthImpl curDepth curNode = curDepth + sum [sumDepthImpl (succ curDepth) nextUp | nextUp <- M.findWithDefault [] curNode forwardMap]

 -- parse, build map parent -> [children], start at COM (guaranteed root) and fan out while incrementing depth and summing
day06a :: String -> String
day06a = show . sumDepth . buildForwardMap . parse

-- parse, build map child -> parent, find full ancestor chain (via unfold) for both YOU and SAN, find first shared element, sum indices
day06b :: String -> String
day06b t = show $ youIndex + sanIndex - 2
  where
    youIndex = fromJust $ elemIndex firstSharedElement youChain
    sanIndex = fromJust $ elemIndex firstSharedElement sanChain
    firstSharedElement = fromJust $ find (`elem` youChain) sanChain
    sanChain = ancestorChain revMap "SAN"
    youChain = ancestorChain revMap "YOU"
    revMap = buildReverseMap (parse t)


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
