{-# LANGUAGE NamedFieldPuns #-}

module Days.Common
  (ComputerState(tape, output), compute, newComputerState, parseComputerTape, newComputerStateParsedTape, nextOutput, appendToInput) where

import Data.List.Split
import Data.Maybe (isJust)

parseComputerTape t = map r $ splitOn "," t where r x = read x::Int

replaceAtIndexsValue l i v = concat [take (l!!i) l, [v], drop ((l!!i) + 1) l]

data ComputerState = ComputerState { position :: Int
                                   , tape     :: [Int]
                                   , input    :: [Int]
                                   , output   :: Maybe Int
                                   , halted   :: Bool
                                   } deriving Show

defaultComputerState = ComputerState { position = 0, tape = undefined, input = [], output = Nothing, halted = False }

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
newComputerState i ts = defaultComputerState { tape = parseComputerTape ts, input = i }

newComputerStateParsedTape :: [Int] -> ComputerState
newComputerStateParsedTape t = defaultComputerState { tape = t }

compute :: ComputerState -> ComputerState
compute = until halted step

nextOutput :: ComputerState -> ComputerState
nextOutput cs = until (\x -> isJust (output x) || halted x ) step cs { output = Nothing }

appendToInput :: Int -> ComputerState -> ComputerState
appendToInput n c = c { input = input c ++ [n] }

step :: ComputerState -> ComputerState
step c@ComputerState{position = i, tape = l, input, output} = case opcode of
  01 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam + secondParam)}
  02 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam * secondParam)}
  03 -> c {position = i + 2, tape = replaceAtIndexsValue l (i+1) (head input), input = tail input}
  04 -> c {position = i + 2, tape = l, input, output = Just firstParam}
  05 -> c {position = if firstParam /= 0 then secondParam else i + 3, tape = l}
  06 -> c {position = if firstParam == 0 then secondParam else i + 3, tape = l}
  07 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam < secondParam then 1 else 0)}
  08 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam == secondParam then 1 else 0)}
  99 -> c { halted = True }
  _  -> error $ "found unexpected instruction: " ++ show (l!!i) ++ " at position " ++ show i
  where
    secondParam = paramModeToAccessor (toParamMode currentVal 1000) (i + 2) l
    firstParam  = paramModeToAccessor (toParamMode currentVal 100) (i + 1) l
    opcode      = mod currentVal 100
    currentVal  = l!!i
