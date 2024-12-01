{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (sort, transpose)

findAnswer :: [[Int]] -> Int
findAnswer cols = sum $ zipWith (\n1 n2 -> abs $ n1 - n2) col1 col2
  where
    [col1, col2] = map sort cols

parseLine :: String -> [Int]
parseLine = map read . words

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cols = transpose $ map parseLine input
  print $ findAnswer cols
