{-# LANGUAGE NamedFieldPuns #-}

module Days.Common where

replaceAtIndex l i v = concat [take i l, [v], drop (i + 1) l]

data ComputerState = ComputerState { position :: Int
                                   , tape     :: [Int]
                                   , input    :: Int
                                   , output   :: Int
                                   }

compute :: ComputerState -> ComputerState
compute ComputerState{position = i, tape = l, input, output}
  | l!!i == 1  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) + l!!(l!!(i + 2))), input, output}
  | l!!i == 2  = compute ComputerState {position = i + 4, tape = replaceAtIndex l (l !! (i + 3)) (l!!(l!!(i + 1)) * l!!(l!!(i + 2))), input, output}
  | l!!i == 99 = ComputerState {position = i, tape = l, input, output}
  | otherwise = error "wah"
