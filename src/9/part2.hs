import Data.Char (digitToInt)

type Size = Int

type ID = Int

data Space = Empty Size | File ID Size deriving (Show, Eq)

isFile :: Space -> Bool
isFile (File _ _) = True
isFile _ = False

expand :: Int -> String -> [Space]
expand _ [] = []
expand i [x] = [File i (digitToInt x)]
expand i (x : y : ys) = File i (digitToInt x) : Empty (digitToInt y) : expand (i + 1) ys

withoutFile :: Space -> [Space] -> [Space]
withoutFile _ [] = []
withoutFile (File fid _) ((File fid' fs) : xs) | fid == fid' = Empty fs : xs
withoutFile f (x : xs) = x : withoutFile f xs

moveFile :: Space -> [Space] -> [Space]
moveFile _ [] = []
moveFile (File fid' _) (x@(File fid _) : xs) | fid == fid' = x : xs -- not moving past itself
moveFile f@(File _ fs) ((Empty es) : xs) | es >= fs = f : Empty (es - fs) : withoutFile f xs -- fits
moveFile f (x : xs) = x : moveFile f xs -- otherwise

defrag :: [Space] -> [Space]
defrag xs = foldr moveFile xs (filter isFile xs)

checksum :: Int -> [Space] -> Int
checksum _ [] = 0
checksum i (Empty r : xs) = checksum (i + r) xs
checksum i (File _ 0 : xs) = checksum i xs
checksum i (File fid r : xs) = i * fid + checksum (i + 1) (File fid (r - 1) : xs)

findAnswer :: String -> Int
findAnswer = checksum 0 . defrag . expand 0

main :: IO ()
main = do
  contents <- getContents
  print $ findAnswer contents
