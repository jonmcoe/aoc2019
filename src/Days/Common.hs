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

newComputerState :: Int -> String -> ComputerState
newComputerState i ts = ComputerState {position = 0, tape = parseComputerTape ts, input = i, output = -1}

newComputerStateParsedTape :: [Int] -> ComputerState
newComputerStateParsedTape t = ComputerState {position = 0, tape = t, input = -1, output = -1}

compute :: ComputerState -> ComputerState -- TODO: traverse or other recursion scheme?
compute ComputerState{position = i, tape = l, input, output}
  | opcode == 01  = compute ComputerState {position = i + 4, tape = replaceAtIndex l thirdParam (firstParam + secondParam), input, output}
  | opcode == 02  = compute ComputerState {position = i + 4, tape = replaceAtIndex l thirdParam (firstParam * secondParam), input, output}
  | opcode == 03  = compute ComputerState {position = i + 2, tape = replaceAtIndex l (l !! (i + 1)) input, input, output}
  | opcode == 04  = compute ComputerState {position = i + 2, tape = l, input, output = firstParam}
  | opcode == 05  = compute ComputerState {position = if firstParam /= 0 then secondParam else i + 3, tape = l, input, output}
  | opcode == 06  = compute ComputerState {position = if firstParam == 0 then secondParam else i + 3, tape = l, input, output}
  | opcode == 07  = compute ComputerState {position = i + 4, tape = replaceAtIndex l thirdParam (if firstParam < secondParam then 1 else 0), input, output}
  | opcode == 08  = compute ComputerState {position = i + 4, tape = replaceAtIndex l thirdParam (if firstParam == secondParam then 1 else 0), input, output}
  | l!!i == 99    = ComputerState {position = i, tape = l, input, output}
  | otherwise = error $ "found unexpected instruction: " ++ show (l!!i) ++ " at position " ++ show i
  where
    thirdParam  = paramModeToAccessor Immediate (i + 3) l -- not sure why we want to hardcode Immediate, but ok. maybe something about replaceAtIndex
    secondParam = paramModeToAccessor (toParamMode currentVal 1000) (i + 2) l
    firstParam  = paramModeToAccessor (toParamMode currentVal 100) (i + 1) l
    opcode      = mod currentVal 100
    currentVal  = l!!i
