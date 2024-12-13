{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)

type Configuration = ((Int, Int), (Int, Int), (Int, Int)) -- button a, button b, prize

minSpend :: Configuration -> Maybe Int
minSpend ((adx, ady), (bdx, bdy), (px, py)) =
  listToMaybe $
    sort [ap * 3 + bp | ap <- [1 .. 100], bp <- [1 .. 100], (adx * ap + bdx * bp, ady * ap + bdy * bp) == (px, py)]

findAnswer :: [Configuration] -> Int
findAnswer = sum . mapMaybe minSpend

parseButton :: String -> (Int, Int)
parseButton raw = (read $ splitOn "+" xr !! 1, read $ splitOn "+" yr !! 1)
  where
    [xr, yr] = splitOn ", " raw

parsePrize :: String -> (Int, Int)
parsePrize raw = (read $ splitOn "=" xr !! 1, read $ splitOn "=" yr !! 1)
  where
    [xr, yr] = splitOn ", " raw

parseConfig :: [String] -> Configuration
parseConfig [a, b, p] = (parseButton a, parseButton b, parsePrize p)
parseConfig _ = error "invalid input"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let configs = map parseConfig $ splitOn [""] input
  print $ findAnswer configs
