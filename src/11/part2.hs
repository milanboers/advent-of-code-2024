import Data.Map (Map)
import qualified Data.Map as Map

evenDigits :: Int -> Bool
evenDigits = even . length . show

apply :: Int -> [Int]
apply 0 = [1]
apply x | evenDigits x = [read $ take half sx, read $ drop half sx]
  where
    sx = show x
    half = length sx `div` 2
apply x = [x * 2024]

applyAll :: Map Int Int -> Map Int Int
applyAll stones = Map.fromListWith (+) [(nx, f) | (x, f) <- Map.toList stones, nx <- apply x]

findAnswer :: Map Int Int -> Int
findAnswer stones = sum $ iterate applyAll stones !! 75

main :: IO ()
main = do
  contents <- getContents
  let stones = Map.fromList [(read w, 1) | w <- words contents]
  print $ findAnswer stones
