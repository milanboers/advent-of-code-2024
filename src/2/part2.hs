sublists :: [Int] -> [[Int]]
sublists [] = []
sublists (x : xs) = xs : [x : ol | ol <- sublists xs]

isSafeInc :: [Int] -> Bool
isSafeInc [] = True
isSafeInc [_] = True
isSafeInc (x : y : ys) = d >= 1 && d <= 3 && isSafeInc (y : ys)
  where
    d = y - x

isSafe :: [Int] -> Bool
isSafe l = isSafeInc l || isSafeInc (reverse l)

isSafeWithDrop :: [Int] -> Bool
isSafeWithDrop l = isSafe l || any isSafe (sublists l)

findAnswer :: [[Int]] -> Int
findAnswer = length . filter isSafeWithDrop

main :: IO ()
main =
  do
    contents <- getContents
    let input = lines contents
    let reports = [[read level | level <- words line] | line <- input]
    print $ findAnswer reports
