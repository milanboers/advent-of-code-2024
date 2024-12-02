isSafeInc :: [Int] -> Bool
isSafeInc [] = True
isSafeInc [_] = True
isSafeInc (x : y : ys) = d >= 1 && d <= 3 && isSafeInc (y : ys)
  where
    d = y - x

isSafe :: [Int] -> Bool
isSafe l = isSafeInc l || isSafeInc (reverse l)

findAnswer :: [[Int]] -> Int
findAnswer = length . filter isSafe

main :: IO ()
main =
  do
    contents <- getContents
    let input = lines contents
    let reports = [[read level | level <- words line] | line <- input]
    print $ findAnswer reports
