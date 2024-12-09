import Data.Char (digitToInt)

expand :: Int -> String -> [Maybe Int]
expand _ [] = []
expand i [x] = replicate (digitToInt x) (Just i)
expand i (x : y : ys) = replicate (digitToInt x) (Just i) ++ replicate (digitToInt y) Nothing ++ expand (i + 1) ys

defrag :: Int -> Int -> [Maybe Int] -> [Maybe Int] -> [Int]
defrag xi yi _ _ | xi == yi = []
defrag xi yi (x : xs) (Nothing : ys) = defrag xi (yi - 1) (x : xs) ys
defrag xi yi (Nothing : xs) ((Just y) : ys) = y : defrag (xi + 1) (yi - 1) xs ys
defrag xi yi ((Just x) : xs) (y : ys) = x : defrag (xi + 1) yi xs (y : ys)
defrag _ _ _ _ = error "exhausted"

checksum :: Int -> [Int] -> Int
checksum _ [] = 0
checksum i (x : xs) = i * x + checksum (i + 1) xs

findAnswer :: String -> Int
findAnswer xs = checksum 0 defragged
  where
    expanded = expand 0 xs
    defragged = defrag 0 (length expanded) expanded (reverse expanded)

main :: IO ()
main = do
  contents <- getContents
  print $ findAnswer contents
