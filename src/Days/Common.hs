{-# LANGUAGE NamedFieldPuns #-}

module Days.Common
  (ComputerState(output), compute, newComputerState, untilOutput, appendToInput, step, subAndCompute) where

import Debug.Trace
import Data.List.Split
import Data.Maybe (isJust)
import qualified Data.Map as M

parseComputerTape :: String -> Tape
parseComputerTape t = M.fromList $ zip [0..] $ map r $ splitOn "," t where r x = read x::Int

replaceAtIndexsValue :: Tape -> Int -> Int -> Tape
replaceAtIndexsValue l i v = M.insert (M.findWithDefault 0 i l) v l

type Tape = M.Map Int Int

data ComputerState = ComputerState { position :: Int
                                   , tape     :: Tape
                                   , input    :: [Int]
                                   , output   :: Maybe Int
                                   , halted   :: Bool
                                   , relative :: Int
                                   } deriving Show

defaultComputerState = ComputerState { position = 0, tape = M.empty, input = [], output = Nothing, halted = False, relative = 0 }

data ParamMode = Positional | Immediate | Relative

paramModeToAccessor :: ParamMode -> Int -> M.Map Int Int -> Int -> Int
paramModeToAccessor paramMode = case paramMode of
  Positional -> \i l _ -> M.findWithDefault 0 (M.findWithDefault 0 i l) l
  Immediate  -> \i l _ -> M.findWithDefault 0 i l
  Relative   -> \i l r -> 7 -- TODO: THIS ISNT BEING CALLED

toParamMode n magnitudeOfInterest
  | d == 0    = Positional
  | d == 1    = Immediate
  | d == 2    = Relative
  | otherwise = error $ show d
  where d = mod (div n magnitudeOfInterest) 10

newComputerState :: [Int] -> String -> ComputerState
newComputerState i ts = defaultComputerState { tape = parseComputerTape ts, input = i }

subAndCompute :: (Int, Int) -> String -> Int
subAndCompute (noun, verb) t = tape endState M.! 0
  where
    endState         = compute substituteState
    substituteState  = defaultComputerState { tape = substitutedTape t }
    substitutedTape  = M.insert 2 verb . M.insert 1 noun . parseComputerTape

compute :: ComputerState -> ComputerState
compute = until halted step

untilOutput :: ComputerState -> ComputerState
untilOutput cs = until (\x -> isJust (output x) || halted x ) step cs { output = Nothing }

appendToInput :: Int -> ComputerState -> ComputerState
appendToInput n c = c { input = input c ++ [n] }

step :: ComputerState -> ComputerState
step c@ComputerState{position = i, tape = l, input, output, relative} = case opcode of
  01 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam + secondParam)} -- third i+3 param needs "r" treatment
  02 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (firstParam * secondParam)}
  03 -> c {position = i + 2, tape = replaceAtIndexsValue l (i+1) (head input), input = tail input}
  04 -> c {position = i + 2, tape = l, input, output = Just firstParam}
  05 -> c {position = if firstParam /= 0 then secondParam else i + 3, tape = l}
  06 -> c {position = if firstParam == 0 then secondParam else i + 3, tape = l}
  07 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam < secondParam then 1 else 0)}
  08 -> c {position = i + 4, tape = replaceAtIndexsValue l (i+3) (if firstParam == secondParam then 1 else 0)}
  09 -> c {position = i + 2, relative = relative + (l M.! (i+1))} -- TODO: looks solid but might be wrong...
  99 -> c { halted = True }
  _  -> error $ "found unexpected instruction: " ++ show (l M.! i) ++ " at position " ++ show i
  where
    secondParam = paramModeToAccessor (toParamMode currentVal 1000) (i + 2) l relative
    firstParam  = paramModeToAccessor (toParamMode currentVal 100) (i + 1) l relative
    opcode      = mod currentVal 100
    currentVal  = M.findWithDefault 0 i l
