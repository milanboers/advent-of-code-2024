import Data.List.Split (splitOn)

findAnswer :: [[Int]] -> [[Int]] -> Int
findAnswer locks keys = length [(key, lock) | key <- keys, lock <- locks, all (<= 5) (zipWith (+) key lock)]

heightmap :: [String] -> [Int]
heightmap = foldr (zipWith (+) . map (fromEnum . (== '#'))) (repeat (-1))

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let rawKeysAndLocks = splitOn [""] input
  let rawLocks = filter (all (== '#') . head) rawKeysAndLocks
  let rawKeys = filter (all (== '.') . head) rawKeysAndLocks
  let locks = map heightmap rawLocks
  let keys = map heightmap rawKeys
  print $ findAnswer locks keys
