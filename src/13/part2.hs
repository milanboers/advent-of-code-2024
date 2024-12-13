{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Configuration = ((Int, Int), (Int, Int), (Int, Int)) -- button a, button b, prize

minSpend :: Configuration -> Maybe Int
minSpend ((adx, ady), (bdx, bdy), (px, py))
  | ar == 0 && br == 0 = Just (3 * a + b)
  | otherwise = Nothing
  where
    (a, ar) = (px * bdy - bdx * py) `divMod` (adx * bdy - bdx * ady)
    (b, br) = (px * ady - adx * py) `divMod` (bdx * ady - adx * bdy)

findAnswer :: [Configuration] -> Int
findAnswer = sum . mapMaybe minSpend

parseButton :: String -> (Int, Int)
parseButton raw = (read $ splitOn "+" xr !! 1, read $ splitOn "+" yr !! 1)
  where
    [xr, yr] = splitOn ", " raw

parsePrize :: String -> (Int, Int)
parsePrize raw = (10000000000000 + read (splitOn "=" xr !! 1), 10000000000000 + read (splitOn "=" yr !! 1))
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
