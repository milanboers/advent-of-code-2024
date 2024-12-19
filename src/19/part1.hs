{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Function (fix)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.MemoUgly (memo)

possible :: [String] -> (String -> Bool) -> String -> Bool
possible _ _ [] = True
possible towels f design = any f $ mapMaybe (`stripPrefix` design) towels

possibleMemo :: [String] -> String -> Bool
possibleMemo towels = fix (memo . possible towels)

findAnswer :: [String] -> [String] -> Int
findAnswer towels = length . filter (possibleMemo towels)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawTowels, designs] = splitOn [""] input
  let towels = splitOn ", " . head $ rawTowels
  print $ findAnswer towels designs
