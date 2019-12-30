{-# LANGUAGE NamedFieldPuns #-}

module Days.Day12 where -- TODO: vector operations, parsing. Libraries to check out: linear, megaparsec, regex

import Data.Char (isDigit)
import Data.List (transpose)
import qualified Data.Set as S

type Vecc = [Int]
data Moon = Moon {pos :: Vecc, vel :: Vecc} deriving (Show, Eq, Ord)

project i Moon{pos, vel} = Moon {pos=[pos!!i], vel=[vel!!i]}

potentialEnergy = sum . map abs . pos
kineticEnergy = sum . map abs . vel
totalEnergy moon = potentialEnergy moon * kineticEnergy moon

parseEntrySimple = map (readInt  . filter (\c -> isDigit c || c == '-')) . words
  where readInt x = read x::Int

--parseEntryRegex = id
--
--parseEntryCombinators = id

parseAll = map (newMoon . parseEntrySimple) . lines
  where newMoon p = Moon {pos=p, vel=replicate (length p) 0}

accel :: Moon -> [Moon] -> Vecc
accel curMoon moons = zipWith singleAccel (pos curMoon) (transpose (map pos moons))
  where
    singleAccel cur others = sum $ map (comp cur) others
    comp a b
      | a == b = 0
      | a < b  = 1
      | a > b  = -1

step :: [Moon] -> [Moon]
step l = map (\(p,v) -> Moon{pos=p, vel=v}) $ zip newPositions newVelocities
  where
    newPositions  = zipWith pairwiseSum newVelocities oldPositions
    newVelocities = zipWith pairwiseSum oldVelocities accelerations
    accelerations = map (`accel` l) l
    oldVelocities = map vel l
    oldPositions  = map pos l
    pairwiseSum   = zipWith (+)

stepsUntilRepeat :: [Moon] -> Int
stepsUntilRepeat = stepsUntilRepeatImpl S.empty
  where
    stepsUntilRepeatImpl acc cur
      | S.member cur acc = length acc
      | otherwise        = stepsUntilRepeatImpl (S.insert cur acc) (step cur)

day12a s = show $ sum $ map totalEnergy $ iterate step (parseAll s) !! 1000
day12b s = show $ foldr (lcm . stepsUntilRepeat . projectAll) 1 [0..2]
  where
    projectAll i = map (project i) moons
    moons = parseAll s