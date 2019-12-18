{-# LANGUAGE NamedFieldPuns #-}

module Days.Common (ComputerState(tape, output), compute, newComputerState, parseComputerTape, newComputerStateParsedTape) where

import Data.List.Split


parseComputerTape t = map r $ splitOn "," t where r x = read x::Int

replaceAtIndex l i v = concat [take i l, [v], drop (i + 1) l]

data ComputerState = ComputerState { position :: Int
                                   , tape     :: [Int]
                                   , input    :: Int
                                   , output   :: Int
                                   }

newComputerState :: Int -> String -> ComputerState
newComputerState i ts = ComputerState {position = 0, tape = parseComputerTape ts, input = i, output = -1}

newComputerStateParsedTape :: [Int] -> ComputerState
newComputerStateParsedTape t = ComputerState {position = 0, tape = t, input = -1, output = -1}

compute :: ComputerState -> ComputerState
compute ComputerState{position = i, tape = l, input, output}
  | l!!i == 3     = compute ComputerState {position = i + 2, tape = replaceAtIndex l (l !! (i + 1)) input, input, output}
  | l!!i == 4     = compute ComputerState {position = i + 2, tape = l, input, output = l!!(l!! (i + 1))}
--  | l!!i == 103     = compute ComputerState {position = i + 2, tape = replaceAtIndex l (l !! (i + 1)) input, input, output}
  | l!!i == 104   = compute ComputerState {position = i + 2, tape = l, input, output = l!!(i + 1)}

  | l!!i == 0001  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) + l!!(l!!(i + 2))), input, output}
  | l!!i == 0002  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) * l!!(l!!(i + 2))), input, output}
  | l!!i == 0101  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(i + 1) + l!!(l!!(i + 2))), input, output}
  | l!!i == 0102  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(i + 1) * l!!(l!!(i + 2))), input, output}
  | l!!i == 1001  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) + l!!(i + 2)), input, output}
  | l!!i == 1002  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) * l!!(i + 2)), input, output}
  | l!!i == 1101  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(i + 1) + l!!(i + 2)), input, output}
  | l!!i == 1102  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(i + 1) * l!!(i + 2)), input, output}

  | l!!i == 99 = ComputerState {position = i, tape = l, input, output}
  | otherwise = error $ "found unexpected instruction: " ++ show (l!!i) ++ " at position " ++ show i
