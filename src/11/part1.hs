evenDigits :: Int -> Bool
evenDigits = even . length . show

apply :: Int -> [Int]
apply 0 = [1]
apply x | evenDigits x = [read $ take half sx, read $ drop half sx]
  where
    sx = show x
    half = length sx `div` 2
apply x = [x * 2024]

applyAll :: [Int] -> [Int]
applyAll = concatMap apply

findAnswer :: [Int] -> Int
findAnswer stones = length $ iterate applyAll stones !! 25

main :: IO ()
main = do
  contents <- getContents
  let stones = map read $ words contents
  print $ findAnswer stones
