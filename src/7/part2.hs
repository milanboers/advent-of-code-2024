{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)

type Equation = (Int, [Int])

cat :: Int -> Int -> Int
cat x y = read $ show x ++ show y

possible :: Equation -> Bool
possible (tv, [x]) = tv == x
possible (tv, x : _) | x > tv = False
possible (tv, x : y : ys) = possible (tv, (x + y) : ys) || possible (tv, (x * y) : ys) || possible (tv, x `cat` y : ys)
possible (_, []) = error "invalid equation"

findAnswer :: [Equation] -> Int
findAnswer = sum . map fst . filter possible

parseEquation :: String -> Equation
parseEquation xs = (read testValue', map read $ splitOn " " nums')
  where
    [testValue', nums'] = splitOn ": " xs

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let equations = map parseEquation input
  print $ findAnswer equations
