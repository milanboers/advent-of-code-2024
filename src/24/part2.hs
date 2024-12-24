{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits (xor)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen)
import System.Random.Stateful (uniformR)
import Text.Printf (printf)

data Op = Xor | Or | And deriving (Show, Eq)

data Expr = Literal Bool | Expr Op String String deriving (Show, Eq)

type Program = Map String Expr

binToDec :: [Bool] -> Int
binToDec = sum . zipWith (\i b -> 2 ^ i * fromEnum b) [0 ..]

decToBin :: Int -> [Bool]
decToBin 0 = []
decToBin n = (m == 1) : decToBin d
  where
    (d, m) = n `divMod` 2

opFunc :: Op -> (Bool -> Bool -> Bool)
opFunc Xor = xor
opFunc Or = (||)
opFunc And = (&&)

eval :: Program -> Set String -> String -> Maybe Bool
eval _ seen es | es `Set.member` seen = Nothing -- cycle
eval p seen es = case p ! es of
  Literal b -> Just b
  Expr op t1 t2 -> case (eval p newSeen t1, eval p newSeen t2) of
    (Just v1, Just v2) -> Just $ opFunc op v1 v2
    _ -> Nothing
  where
    newSeen = Set.insert es seen

getAll :: [Maybe a] -> Maybe [a]
getAll [] = Just []
getAll (Nothing : _) = Nothing
getAll ((Just x) : xs) = (x :) <$> getAll xs

evalIO :: Program -> Char -> Maybe [Bool]
evalIO p c = getAll . map (eval p Set.empty) . filter ((== c) . head) $ Map.keys p

evalOutput :: Program -> Int -> Maybe Bool
evalOutput p i = (!! i) <$> evalIO p 'z'

inputLabel :: Char -> Int -> String
inputLabel c = printf (c : "%02d")

wiresForExpr :: Program -> Expr -> Set String
wiresForExpr _ (Literal _) = Set.empty
wiresForExpr p (Expr _ w1 w2) =
  Set.insert w1 . Set.insert w2 $
    Set.union (wiresForExpr p (p ! w1)) (wiresForExpr p (p ! w2))

wiresFori :: Program -> Int -> Set String
wiresFori p oi = Map.keysSet . Map.filter ((> 0) . Set.size . Set.intersection inputs . wiresForExpr p) $ p
  where
    inputs = Set.fromList [inputLabel 'x' oi, inputLabel 'y' oi]

setInput :: Char -> Int -> Program -> Program
setInput c v =
  Map.union
    . Map.fromList
    . zipWith (\i b -> (inputLabel c i, Literal b)) [0 :: Int ..]
    $ decToBin v

setInputs :: Program -> Int -> Int -> Program
setInputs p x y = foldr (\(c, v) p' -> setInput c v p') p [('x', x), ('y', y)]

swap :: Program -> (String, String) -> Program
swap p (k1, k2) = Map.insert k2 (p ! k1) . Map.insert k1 (p ! k2) $ p

workingSwap :: StdGen -> Program -> Int -> [(String, String)] -> (String, String)
workingSwap _ _ _ [] = error "empty list"
workingSwap _ _ _ [swp] = swp
workingSwap rng p oi swps = workingSwap rng'' p oi stillValid
  where
    (n1, rng') = uniformR (2 ^ 45, 2 ^ 46) rng
    (n2, rng'') = uniformR (2 ^ 45, 2 ^ 46) rng'
    expected = decToBin (n1 + n2) !! oi
    stillValid = [swp | swp <- swps, evalOutput (setInputs (swap p swp) n1 n2) oi == Just expected]

swapUntilGood :: Program -> [Bool] -> Int -> [(String, String)]
swapUntilGood _ [] _ = []
swapUntilGood p (e : es) oi | evalOutput p oi == Just e = swapUntilGood p es (oi + 1)
swapUntilGood p (_ : es) oi = toSwap : swapUntilGood (swap p toSwap) es (oi + 1)
  where
    newWires = wiresFori p oi `Set.difference` wiresFori p (oi + 1)
    toSwap =
      workingSwap
        (mkStdGen 42)
        p
        oi
        [(w1, w2) | w1 <- Set.toList newWires, w2 <- Set.toList newWires, w1 < w2]

findAnswer :: Program -> String
findAnswer p = intercalate "," . sort . concatMap (\(x, y) -> [x, y]) $ swapUntilGood p expected 0
  where
    xs = fromJust $ evalIO p 'x'
    ys = fromJust $ evalIO p 'y'
    expected = decToBin (binToDec xs + binToDec ys)

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
