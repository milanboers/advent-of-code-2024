{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Function (fix)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.MemoUgly (memo)

possible :: [String] -> (String -> Int) -> String -> Int
possible _ _ [] = 1
possible towels f design = sum . map f $ mapMaybe (`stripPrefix` design) towels

possibleMemo :: [String] -> String -> Int
possibleMemo towels = fix (memo . possible towels)

findAnswer :: [String] -> [String] -> Int
findAnswer towels = sum . map (possibleMemo towels)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawTowels, designs] = splitOn [""] input
  let towels = splitOn ", " . head $ rawTowels
  print $ findAnswer towels designs
