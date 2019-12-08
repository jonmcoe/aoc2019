module Days.Day04 where

import Data.Char
import qualified Data.Map.Strict as M
import Data.List.Split

toDigits = map digitToInt . show
parse = map (\x -> read  x :: Int) . splitOn "-"

nonDescending :: Ord a => [a] -> Bool
nonDescending l = case l of
  x:y:rest -> x <= y && nonDescending (y:rest)
  _  -> True

frequencies = foldl (\tally incoming -> if M.member incoming tally then M.adjust (+ 1) incoming tally else M.insert incoming 1 tally ) M.empty
hasRepeat = any (< 1) . M.elems . frequencies
hasTwopeat = elem 2 . M.elems . frequencies

day04a t = show $ length $ filter (\x -> nonDescending x && hasRepeat x) (map toDigits [bottom..top])
  where bottom:top:_ = parse t
day04b t = show $ length $ filter (\x -> nonDescending x && hasTwopeat x) (map toDigits [bottom..top])
  where bottom:top:_ = parse t
