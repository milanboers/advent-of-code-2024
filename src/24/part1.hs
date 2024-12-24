{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits (xor)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Op = Xor | Or | And deriving (Show, Eq)

data Expr = Literal Bool | Expr Op String String deriving (Show, Eq)

type Program = Map String Expr

binToDec :: [Bool] -> Int
binToDec = sum . zipWith (\i b -> 2 ^ i * fromEnum b) [0 ..]

eval :: Program -> String -> Bool
eval p es = case p ! es of
  Literal b -> b
  Expr Xor t1 t2 -> eval p t1 `xor` eval p t2
  Expr And t1 t2 -> eval p t1 && eval p t2
  Expr Or t1 t2 -> eval p t1 || eval p t2

findAnswer :: Program -> Int
findAnswer p = binToDec . map (eval p) . filter ((== 'z') . head) $ Map.keys p

parseLiteral :: String -> (String, Expr)
parseLiteral xs = (name, Literal (val == "1"))
  where
    [name, val] = splitOn ": " xs

parseOp :: String -> Op
parseOp "XOR" = Xor
parseOp "AND" = And
parseOp "OR" = Or
parseOp _ = error "invalid operator"

parseExpr :: String -> (String, Expr)
parseExpr xs = (name, Expr (parseOp op) t1 t2)
  where
    [expr, name] = splitOn " -> " xs
    [t1, op, t2] = splitOn " " expr

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawLits, rawExprs] = splitOn [""] input
  let literals = map parseLiteral rawLits
  let exprs = map parseExpr rawExprs
  let program = Map.fromList (literals ++ exprs)
  print $ findAnswer program
