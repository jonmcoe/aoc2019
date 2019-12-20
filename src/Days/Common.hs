{-# LANGUAGE NamedFieldPuns #-}

module Days.Common (ComputerState(tape, output), compute, newComputerState, parseComputerTape, newComputerStateParsedTape) where

import Data.List.Split


parseComputerTape t = map r $ splitOn "," t where r x = read x::Int

replaceAtIndexsValue l i v = concat [take (l!!i) l, [v], drop ((l!!i) + 1) l]

data ComputerState = ComputerState { position :: Int
                                   , tape     :: [Int]
                                   , input    :: [Int]
                                   , output   :: Int
                                   }

data ParamMode = Positional | Immediate

paramModeToAccessor :: ParamMode -> Int -> [Int] -> Int
paramModeToAccessor paramMode = case paramMode of
  Positional -> \i l -> l!!(l!!i)
  Immediate  -> flip (!!)

toParamMode n magnitudeOfInterest
  | d == 0    = Positional
  | d == 1    = Immediate
  | otherwise = error $ show d
  where d = mod (div n magnitudeOfInterest) 10

newComputerState :: [Int] -> String -> ComputerState
newComputerState i ts = ComputerState {position = 0, tape = parseComputerTape ts, input = i, output = -1}

newComputerStateParsedTape :: [Int] -> ComputerState
newComputerStateParsedTape t = ComputerState {position = 0, tape = t, input = [-1], output = -1}

compute :: ComputerState -> ComputerState -- TODO: traverse or other recursion scheme?
compute c@ComputerState{position = i, tape = l, input, output} = case opcode of
  01 -> compute ComputerState {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam + secondParam), input, output}
  02 -> compute ComputerState {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam * secondParam), input, output}
  03 -> compute ComputerState {position = i + 2, tape = replaceAtIndexsValue l (i+1) (head input), input = tail input, output}
  04 -> compute ComputerState {position = i + 2, tape = l, input, output = firstParam}
  05 -> compute ComputerState {position = if firstParam /= 0 then secondParam else i + 3, tape = l, input, output}
  06 -> compute ComputerState {position = if firstParam == 0 then secondParam else i + 3, tape = l, input, output}
  07 -> compute ComputerState {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam < secondParam then 1 else 0), input, output}
  08 -> compute ComputerState {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam == secondParam then 1 else 0), input, output}
  99 -> c
  _  -> error $ "found unexpected instruction: " ++ show (l!!i) ++ " at position " ++ show i
  where
    secondParam = paramModeToAccessor (toParamMode currentVal 1000) (i + 2) l
    firstParam  = paramModeToAccessor (toParamMode currentVal 100) (i + 1) l
    opcode      = mod currentVal 100
    currentVal  = l!!i
