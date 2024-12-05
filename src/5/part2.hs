{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

type Rules = Map (Int, Int) Ordering

type Page = [Int]

sortedPage :: Rules -> Page -> Page
sortedPage rules = sortBy (\x y -> Map.findWithDefault EQ (x, y) rules)

middleNum :: Page -> Int
middleNum xs = xs !! (length xs `div` 2)

findAnswer :: Rules -> [Page] -> Int
findAnswer rules pages = sum [middleNum sorted | page <- pages, let sorted = sortedPage rules page, sorted /= page]

parsePage :: String -> Page
parsePage = map read . splitOn ","

parseRule :: String -> (Int, Int)
parseRule xs = (v1, v2)
  where
    [v1, v2] = map read $ splitOn "|" xs

parseRules :: [String] -> Rules
parseRules xs = Map.fromList [r | x <- xs, let (v1, v2) = parseRule x, r <- [((v1, v2), LT), ((v2, v1), GT)]]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawRules, rawPages] = splitOn [""] input
  let rules = parseRules rawRules
  let pages = map parsePage rawPages
  print $ findAnswer rules pages
