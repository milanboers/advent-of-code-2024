{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (sort, transpose)

findAnswer :: [[Int]] -> Int
findAnswer cols = sum $ [x * length (filter (== x) col2) | x <- col1]
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
