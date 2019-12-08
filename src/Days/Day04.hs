module Days.Day04 where

import Data.Char
import Data.List.Split
import Data.List (group)

parse = map (\x -> read  x :: Int) . splitOn "-"

nonDescending :: Ord a => [a] -> Bool
nonDescending l = case l of
  x:y:rest -> x <= y && nonDescending (y:rest)
  _  -> True

hasRepeat = any (> 1) . map length . group
hasTwopeat = elem 2 . map length . group

day04a t = show $ length $ filter (\x -> nonDescending x && hasRepeat x) (map show [bottom..top])
  where bottom:top:_ = parse t
day04b t = show $ length $ filter (\x -> nonDescending x && hasTwopeat x) (map show [bottom..top])
  where bottom:top:_ = parse t
