{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits (xor)
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Instruction = (Int, Int) -- opcode, operand

type Registers = Map Char Int

combo :: Registers -> Int -> Int
combo regs 4 = regs ! 'A'
combo regs 5 = regs ! 'B'
combo regs 6 = regs ! 'C'
combo _ x = x

run :: [Instruction] -> Registers -> [Instruction] -> [Int]
run ins regs ((0, o) : ps) = run ins (Map.insert 'A' (regs ! 'A' `div` (2 ^ combo regs o)) regs) ps
run ins regs ((1, o) : ps) = run ins (Map.insert 'B' (regs ! 'B' `xor` o) regs) ps
run ins regs ((2, o) : ps) = run ins (Map.insert 'B' (combo regs o `mod` 8) regs) ps
run ins regs ((3, _) : ps) | regs ! 'A' == 0 = run ins regs ps
run ins regs ((3, o) : _) = run ins regs (drop o ins)
run ins regs ((4, _) : ps) = run ins (Map.insert 'B' (regs ! 'B' `xor` regs ! 'C') regs) ps
run ins regs ((5, o) : ps) = (combo regs o `mod` 8) : run ins regs ps
run ins regs ((6, o) : ps) = run ins (Map.insert 'B' (regs ! 'A' `div` (2 ^ combo regs o)) regs) ps
run ins regs ((7, o) : ps) = run ins (Map.insert 'C' (regs ! 'A' `div` (2 ^ combo regs o)) regs) ps
run _ _ _ = []

runWithRegA :: Registers -> [Instruction] -> Int -> [Int]
runWithRegA regs ins a = run ins (Map.insert 'A' a regs) ins

findAnswer :: Registers -> [Instruction] -> Int -> Int
findAnswer regs ins i
  | result == flatIns = i
  | result `isSuffixOf` flatIns = findAnswer regs ins (i * 8)
  | otherwise = findAnswer regs ins (i + 1)
  where
    flatIns = concatMap (\(x, y) -> [x, y]) ins
    result = runWithRegA regs ins i

tuplify :: [Int] -> (Int, Int)
tuplify [x, y] = (x, y)
tuplify _ = error "list not of size 2"

parseRegister :: String -> Int
parseRegister = read . (!! 1) . splitOn ": "

parseProgram :: String -> [(Int, Int)]
parseProgram = map tuplify . chunksOf 2 . map read . splitOn "," . (!! 1) . splitOn ": "

main :: IO ()
main = do
  contents <- getContents
  let [regA, regB, regC, _, prog] = lines contents
  let registers = Map.fromList . zip ['A', 'B', 'C'] $ map parseRegister [regA, regB, regC]
  let program = parseProgram prog
  print $ findAnswer registers program 1
