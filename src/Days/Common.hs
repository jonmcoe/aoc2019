{-# LANGUAGE NamedFieldPuns #-}

module Days.Common
  (ComputerState(output), appendToInput, getTapeAtIndex, newComputerState, step, untilHalt, untilOutput) where

import Debug.Trace
import Data.List.Split
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M


type Tape = M.Map Int Int

data ComputerState = ComputerState { position :: Int
                                   , tape     :: Tape
                                   , input    :: [Int]
                                   , output   :: Maybe Int
                                   , halted   :: Bool
                                   , relative :: Int
                                   } deriving Show

getTapeAtIndex :: Int -> ComputerState -> Int
getTapeAtIndex i cs = tape cs M.! i

parseComputerTape :: String -> [(Int, Int)] -> Tape
parseComputerTape t replacements = M.union replaceMap parsedMap
  where
    parsedMap  = M.fromList $ zip [0..] $ map r $ splitOn "," t where r x = read x::Int
    replaceMap = M.fromList replacements

defaultComputerState = ComputerState { position = 0, tape = M.empty, input = [], output = Nothing, halted = False, relative = 0 }

data ParamMode = Positional | Immediate | Relative

paramModeToAccessor :: ParamMode -> Int -> M.Map Int Int -> Int -> Int
paramModeToAccessor paramMode = case paramMode of
  Positional -> \i l _ -> l M.! i
  Immediate  -> \i l _ -> i
  Relative   -> \i l r -> (l M.! i) + r

toParamMode n magnitudeOfInterest
  | d == 0    = Positional
  | d == 1    = Immediate
  | d == 2    = Relative
  | otherwise = error $ show d
  where d = mod (div n magnitudeOfInterest) 10

newComputerState :: [Int] -> [(Int,Int)] -> String -> ComputerState
newComputerState i replacements ts = defaultComputerState { tape = parseComputerTape ts replacements, input = i }

untilHalt :: ComputerState -> ComputerState
untilHalt = until halted step

untilOutput :: ComputerState -> ComputerState
untilOutput cs = until (\x -> isJust (output x) || halted x ) step cs { output = Nothing }

appendToInput :: Int -> ComputerState -> ComputerState
appendToInput n c = c { input = input c ++ [n] }

step :: ComputerState -> ComputerState
step c@ComputerState{position = i, tape = l, input, output, relative} = case opcode of
  01 -> c {position = i + 4, tape = M.insert thirdParamIndex (firstParam + secondParam) l}
  02 -> c {position = i + 4, tape = M.insert thirdParamIndex (firstParam * secondParam) l}
  03 -> c {position = i + 2, tape = M.insert firstParamIndex (head input) l, input = tail input}
  04 -> c {position = i + 2, tape = l, input, output = Just firstParam}
  05 -> c {position = if firstParam /= 0 then secondParam else i + 3, tape = l}
  06 -> c {position = if firstParam == 0 then secondParam else i + 3, tape = l}
  07 -> c {position = i + 4, tape = M.insert thirdParamIndex (if firstParam < secondParam then 1 else 0) l}
  08 -> c {position = i + 4, tape = M.insert thirdParamIndex (if firstParam == secondParam then 1 else 0) l}
  09 -> c {position = i + 2, relative = relative + firstParam }
  99 -> c { halted = True }
  _  -> error $ "found unexpected instruction: " ++ show (l M.! i) ++ " at position " ++ show i
  where
    secondParam      = M.findWithDefault 0 secondParamIndex l
    firstParam       = M.findWithDefault 0 firstParamIndex l
    thirdParamIndex  = paramModeToAccessor (toParamMode currentVal 10000) (i + 3) l relative
    secondParamIndex = paramModeToAccessor (toParamMode currentVal 1000) (i + 2) l relative
    firstParamIndex  = paramModeToAccessor (toParamMode currentVal 100) (i + 1) l relative
    opcode           = mod currentVal 100
    currentVal       = l M.! i
